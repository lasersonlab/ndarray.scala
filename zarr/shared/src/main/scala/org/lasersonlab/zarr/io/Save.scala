package org.lasersonlab.zarr.io

import cats.implicits._
import io.circe.Encoder
import lasersonlab.xscala._
import lasersonlab.zarr.Path
import magnolia._
import org.lasersonlab.zarr.{ Group, MonadErr }
import org.lasersonlab.zarr.circe.auto._
import org.lasersonlab.zarr.circe.pprint

import scala.language.experimental.macros

trait Save[T] {
  def apply[F[_]: MonadErr](t: T, path: Path[F]): F[Unit] = direct(t, path)
  protected def direct[F[_]: MonadErr](t: T, dir: Path[F]): F[Unit]
}

trait LowPrioritySave
{
  self: Save.type ⇒

  type Typeclass[T] = Save[T]

  /** defines equality for this case class in terms of equality for all its parameters */
  def combine[T](ctx: CaseClass[Save, T]): Save[T] =
    new Save[T] {
      def direct[F[_]: MonadErr](t: T, dir: Path[F]): F[Unit] =
        (
          Group.Metadata().save(dir) ::
          ctx
            .parameters
            .toList
            .map {
              param ⇒
                param.typeclass(
                  param.dereference(t),
                  dir / param.label
                )
            }
        )
        .sequence
        .map { _ ⇒ () }
    }

  /** choose which equality subtype to defer to
   *
   *  Note that in addition to dispatching based on the type of the first parameter to the `equal`
   *  method, we check that the second parameter is the same type. */
  def dispatch[T](ctx: SealedTrait[Save, T]): Save[T] =
    new Save[T] {
      def direct[F[_]: MonadErr](t: T, dir: Path[F]): F[Unit] =
        ctx.dispatch(t) {
          sub ⇒
            sub.typeclass(
              sub.cast(t),
              dir
            )
        }
    }

  /** binds the Magnolia macro to the `gen` method */
  implicit def gen[T]: Save[T] = macro Magnolia.gen[T]

  //def apply[F[_]: MonadErr, T](fn: (T, Path[F]) ⇒ F[Unit]): Save[T] = new Save[T] { def direct[F[_]: MonadErr](t: T, dir: Path[F]): F[Unit] = fn(t, dir) }
  def as[F[_]: MonadErr, L, R: Save](fn: L ⇒ R): Save[L] =
    new Save[L] {
      def direct[F[_] : MonadErr](t: L, dir: Path[F]): F[Unit] =
        fn(t).save(dir)
    }
}

object Save
  extends LowPrioritySave
{

  implicit def withBasenameAsJSON[T](
    implicit
    basename: Basename[T],
    encoder: Encoder[T]
  ):
    Save[T] =
    new Save[T] {
      def direct[F[_] : MonadErr](t: T, dir: Path[F]): F[Unit] = {
        val path = dir / basename
        path
          .write(
            pprint(
              encoder(t)
            )
          )
      }
    }

  implicit def opt[T](implicit save: Save[T]): Save[Option[T]] =
    new Save[Option[T]] {
      def direct[F[_] : MonadErr](t: Option[T], dir: Path[F]): F[Unit] =
        t
          .fold {
            (()).pure[F]
          } {
            save(_, dir)
          }
    }

  implicit class Ops[T](val t: T) extends AnyVal {
    def save[F[_]: MonadErr](dir: Path[F])(implicit save: Save[T]): F[Unit] = save(t, dir)
  }

  trait syntax {
    @inline implicit def zarrSaveOps[T](t: T) = Ops(t)
  }
}
