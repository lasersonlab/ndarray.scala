package org.lasersonlab.zarr.io

import cats.implicits._
import hammerlab.either._
import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder
import io.circe.parser.parse
import magnolia._
import scala.language.experimental.macros

import scala.collection.mutable.ArrayBuffer

trait Load[T] {
  def apply(dir: Path): Exception | T
}

trait LowPriorityLoad {
  implicit def basename[T](
    implicit
    basename: Basename[T],
    decoder: Decoder[T]
  ):
    Load[T] =
    new Load[T] {
      def apply(dir: Path): Exception | T =
        dir ? basename flatMap {
          path ⇒
            parse(path.read)
              .flatMap {
                decoder.decodeJson
              }
        }
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
      def apply(dir: Path): Exception | Option[T] =  {
        val path = dir / basename
        if (!path.exists)
          Right(None)
        else
          parse(path.read)
            .flatMap {
              json ⇒
                decoder
                  .decodeJson(json)
            }
            .map { Some(_) }
      }
    }

  type Typeclass[T] = Load[T]

  /** defines equality for this case class in terms of equality for all its parameters */
  def combine[T](ctx: CaseClass[Load, T]): Load[T] =
    new Load[T] {
      def apply(dir: Path) =
        ctx
          .parameters
          .toList
          .map {
            param ⇒
              param.typeclass(
                dir / param.label
              )
          }
          .sequence
          .map {
            results ⇒
              ctx.rawConstruct(results)
          }
    }

  case class CoproductLoadError(path: Path, exceptions: Seq[(String, Exception)]) extends RuntimeException
  case class CoproductAmbiguousLoadError(path: Path, results: Seq[String]) extends RuntimeException

  /** choose which equality subtype to defer to
   *
   *  Note that in addition to dispatching based on the type of the first parameter to the `equal`
   *  method, we check that the second parameter is the same type. */
  def dispatch[T](ctx: SealedTrait[Load, T]): Load[T] =
    new Load[T] {
      def apply(dir: Path) = {
        val exceptions = ArrayBuffer[(String, Exception)]()
        val  successes = ArrayBuffer[(String,         T)]()
        ctx
          .subtypes
          .foreach {
            t ⇒
              val name = t.typeName.short
              t
                .typeclass(dir)
                .fold(
                  exceptions += name → _,
                   successes += name → _
                )
          }

        successes match {
          case ArrayBuffer((_, success)) ⇒ Right(success)
          case ArrayBuffer() ⇒ Left(CoproductLoadError(dir, exceptions))
          case _ ⇒ Left(CoproductAmbiguousLoadError(dir, successes.map(_._1)))
        }
      }
    }

  /** binds the Magnolia macro to the `gen` method */
  implicit def gen[T]: Load[T] = macro Magnolia.gen[T]

  implicit class Ops(val dir: Path) extends AnyVal {
    def load[T](implicit l: Load[T]): Exception | T = l(dir)
  }
  trait syntax {
    @inline implicit def zarrLoadOps(dir: Path) = Ops(dir)
  }
}
