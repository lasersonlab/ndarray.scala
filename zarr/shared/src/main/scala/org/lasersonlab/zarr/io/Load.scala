package org.lasersonlab.zarr.io

import cats.effect.IO
import cats.{ FlatMap, Monad, Traverse }
import cats.implicits._
import hammerlab.either._
import io.circe.Decoder
import io.circe.parser.parse
import lasersonlab.zarr.Path
import magnolia._
import org.lasersonlab.zarr.MonadErr

import scala.language.experimental.macros
import scala.collection.mutable.ArrayBuffer

trait Load[T] {
  def apply[F[_]: MonadErr](dir: Path[F]): F[T]
}

trait LowPriorityLoad {
  implicit def basename[T](
    implicit
    basename: Basename[T],
    decoder: Decoder[T]
  ):
    Load[T] =
    new Load[T] {
      def apply[F[_]: MonadErr](dir: Path[F]): F[T] =
        (dir ? basename)
          .flatMap {
            _
              .string
              .map(
                parse(_)
                  .flatMap {
                    decoder.decodeJson(_): Exception | T
                  }
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
      def apply[F[_]: MonadErr](dir: Path[F]): F[Option[T]] = {
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
                    parse(_)
                      .flatMap {
                        decoder
                          .decodeJson(_): Exception | T
                      }
                      .map { Some(_): Option[T] }
                  }
                  .rethrow
          }
      }
    }

  //type F[T] = IO[T]
  type Typeclass[T] = Load[T]

  /** defines equality for this case class in terms of equality for all its parameters */
  def combine[T](ctx: CaseClass[Load, T]): Typeclass[T] =
    new Typeclass[T] {
      def apply[F[_]: MonadErr](dir: Path[F]) =
        ctx
          .parameters
          .toList
          .map {
            param ⇒
              param.typeclass.apply(
                dir / param.label
              ).map(x ⇒ x: Any)
          }
          .sequence[F, Any]
          .map {
            results ⇒
              ctx.rawConstruct(results)
          }
    }

  case class CoproductLoadError         [F[_]](path: Path[F], exceptions: Seq[(String, Exception)]) extends RuntimeException
  case class CoproductAmbiguousLoadError[F[_]](path: Path[F],    results: Seq[ String            ]) extends RuntimeException

  /** choose which equality subtype to defer to
   *
   *  Note that in addition to dispatching based on the type of the first parameter to the `equal`
   *  method, we check that the second parameter is the same type. */
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] =
    new Typeclass[T] {
      def apply[F[_]: MonadErr](dir: Path[F]): F[T] =
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
              List[(String, Exception)](),
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

  implicit class Ops[F[_]](val dir: Path[F]) extends AnyVal {
    def load[T](implicit l: Load[T], F: MonadErr[F]): F[T] = l(dir)
  }
  trait syntax {
    @inline implicit def zarrLoadOps[F[_]](dir: Path[F]) = Ops(dir)
  }
}
