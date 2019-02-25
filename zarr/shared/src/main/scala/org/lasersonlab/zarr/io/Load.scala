package org.lasersonlab.zarr.io

import java.io.IOException

import cats.implicits._
import hammerlab.either._
import io.circe.Decoder
import io.circe.parser.parse
import lasersonlab.zarr.{ F, Path }
import magnolia._

import scala.language.experimental.macros
import scala.concurrent.ExecutionContext

trait Load[T] {
  def apply(dir: Path)(implicit ec: ExecutionContext): F[T]
}

trait LowPriorityLoad {
  case class LoadFailure(path: Path, contents: String, cause: Throwable) extends IOException(s"Failed to load $path: $contents", cause)
  implicit def basename[T](
    implicit
    basename: Basename[T],
    decoder: Decoder[T]
  ):
    Load[T] =
    new Load[T] {
      def apply(dir: Path)(implicit ec: ExecutionContext): F[T] =
        (dir ? basename)
          .flatMap {
            _
              .string
              .map(
                str ⇒
                  parse(str)
                    .flatMap {
                      decoder.decodeJson
                    }
                    .left
                    .map { LoadFailure(dir, str, _): Throwable }
              )
          }
          .rethrow
    }
}
object Load
  extends LowPriorityLoad {
  implicit def basenameOpt[T](
    implicit
    basename: Basename[T],
    decoder: Decoder[T]
  ):
    Load[Option[T]] =
    new Load[Option[T]] {
      def apply(dir: Path)(implicit ec: ExecutionContext): F[Option[T]] = {
        val path = dir / basename
        path
          .exists
          .flatMap {
            exists ⇒
              if (!exists)
                Option.empty[T].pure[F]
              else
                path
                  .string
                  .map {
                    str ⇒
                      parse(str)
                        .flatMap { decoder.decodeJson }
                        .left
                        .map { LoadFailure(dir, str, _): Throwable }
                        .map { Some(_): Option[T] }
                  }
                  .rethrow
          }
      }
    }

  type Typeclass[T] = Load[T]

  /** defines equality for this case class in terms of equality for all its parameters */
  def combine[T](ctx: CaseClass[Load, T]): Typeclass[T] =
    new Typeclass[T] {
      def apply(dir: Path)(implicit ec: ExecutionContext) =
        ctx
          .parameters
          .toList
          .map {
            param ⇒
              param
                .typeclass
                .apply(
                  dir / param.label
                )
                .map(x ⇒ x: Any)
          }
          .sequence[F, Any]
          .map {
            results ⇒
              ctx.rawConstruct(results)
          }
    }

  case class CoproductLoadError         (path: Path, exceptions: Seq[(String, Throwable)]) extends RuntimeException
  case class CoproductAmbiguousLoadError(path: Path,    results: Seq[ String            ]) extends RuntimeException

  /** choose which equality subtype to defer to
   *
   *  Note that in addition to dispatching based on the type of the first parameter to the `equal`
   *  method, we check that the second parameter is the same type. */
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] =
    new Typeclass[T] {
      def apply(dir: Path)(implicit ec: ExecutionContext): F[T] =
        ctx
          .subtypes
          .map {
            t ⇒
              (
                t
                  .typeName
                  .short,
                t
                  .typeclass(dir)
                  .attempt
              )
          }
          .foldLeft(
            (
              List[(String, Throwable)](),
              List[(String,         T)]()
            )
            .pure[F]
          ) {
            case (results, (name, t)) ⇒
              t
                .flatMap {
                  t ⇒
                    results
                      .map {
                        case (exceptions, successes) ⇒
                          t.fold(
                            e ⇒ ((name, e) :: exceptions, successes),
                            s ⇒ (exceptions, (name, s) :: successes)
                          )
                      }
                }
          }
          .map {
            case (Nil, (_, success) :: Nil) ⇒ success
            case (Nil, successes) ⇒ throw CoproductAmbiguousLoadError(dir, successes.map(_._1))
            case (exceptions, _) ⇒ throw CoproductLoadError(dir, exceptions)
          }
    }

  /** binds the Magnolia macro to the `gen` method */
  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

  implicit class Ops(val dir: Path) extends AnyVal {
    def load[T](implicit l: Load[T], ec: ExecutionContext): F[T] = l(dir)
  }
  trait syntax {
    @inline implicit def zarrLoadOps(dir: Path)(implicit ec: ExecutionContext) = Ops(dir)
  }
}
