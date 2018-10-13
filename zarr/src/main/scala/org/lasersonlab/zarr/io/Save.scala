package org.lasersonlab.zarr.io

import cats.implicits._
import hammerlab.either._
import hammerlab.option._
import hammerlab.path._
import io.circe.Encoder
import lasersonlab.xscala._
import magnolia._
import org.lasersonlab.zarr.pprint
import scala.language.experimental.macros

import scala.util.Try

trait Save[T] {
  def apply(t: T, dir: Path): Throwable | Unit
}

trait LowPrioritySave {

  type Typeclass[T] = Save[T]

  /** defines equality for this case class in terms of equality for all its parameters */
  def combine[T](ctx: CaseClass[Save, T]): Save[T] =
    new Save[T] {
      def apply(value1: T, dir: Path) =
        ctx
          .parameters
          .toList
          .map {
            param ⇒
              param.typeclass(
                param.dereference(value1),
                dir / param.label
              )
          }
          .sequence
          .map { _ ⇒ () }
    }

  /** choose which equality subtype to defer to
   *
   *  Note that in addition to dispatching based on the type of the first parameter to the `equal`
   *  method, we check that the second parameter is the same type. */
  def dispatch[T](ctx: SealedTrait[Save, T]): Save[T] =
    new Save[T] {
      def apply(value1: T, dir: Path) =
        ctx.dispatch(value1) {
          sub ⇒
            sub.typeclass(
              sub.cast(value1),
              dir
            )
        }
    }

  /** binds the Magnolia macro to the `gen` method */
  implicit def gen[T]: Save[T] = macro Magnolia.gen[T]
}

trait BasenameSave
  extends LowPrioritySave {

  implicit def withBasenameAsJSON[T](
    implicit
    basename: Basename[T],
    encoder: Encoder[T]
  ):
    Save[T] =
    new Save[T] {
      def apply(t: T, dir: Path): Throwable | Unit =
        Try {
          val path = dir / basename
          path.mkdirs
          path
            .write(
              pprint(
                encoder(t)
              )
            )
        }
        .toEither
    }
}

object Save
  extends BasenameSave {

  implicit def opt[T](implicit save: Save[T]): Save[Option[T]] =
    new Save[Option[T]] {
      def apply(t: Option[T], dir: Path): Throwable | Unit =
        t match {
          case Some(t) ⇒ save(t, dir)
          case _ ⇒ Right(())
        }
    }

  implicit class Ops[T](val t: T) extends AnyVal {
    def save(dir: Path)(implicit save: Save[T]): Throwable | Unit = save(t, dir)
  }

  trait syntax {
    @inline implicit def zarrSaveOps[T](t: T) = Ops(t)
  }
}
