package org.lasersonlab.zarr.io

import java.nio.file.Files.{ createTempDirectory, move }
import java.nio.file.StandardCopyOption._

import cats.implicits._
import hammerlab.either._
import hammerlab.path._
import io.circe.Encoder
import lasersonlab.xscala._
import magnolia._
import org.lasersonlab.zarr.Group
import org.lasersonlab.zarr.circe.auto._
import org.lasersonlab.zarr.circe.pprint
import org.lasersonlab.zarr.io.Save.Atomic
import org.lasersonlab.zarr.io.Save.Atomic._

import scala.language.experimental.macros
import scala.util.Try

trait Save[T] {
  def apply(t: T, path: Path)(implicit atomic: Atomic): Throwable | Unit =
    if (atomic == yes && (path.uri.getScheme == null || path.uri.getScheme == "file")) {
      val tmp = Path(createTempDirectory(s"tmp-${path.basename}"))
      val out = tmp / 'out
      val result =
        for {
          _ ← direct(t, out)
          _ ←
            Try {
              if (out.isDirectory)
                (path / 'foo) mkdirs
              else if (out.isFile)
                path.mkdirs

              if (out.exists)
                move(out, path)
            }
            .toEither
        } yield
          ()

      tmp.delete(recursive = true)

      result
    } else
      direct(t, path)

  implicit val atomic: Atomic = no

  protected def direct(t: T, dir: Path): Throwable | Unit
}

trait LowPrioritySave
  extends Pri2
{
  self: Save.type ⇒

  type Typeclass[T] = Save[T]

  /** defines equality for this case class in terms of equality for all its parameters */
  def combine[T](ctx: CaseClass[Save, T]): Save[T] =
    new Save[T] {
      def direct(t: T, dir: Path): Throwable | Unit =
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
      def direct(t: T, dir: Path): Throwable | Unit =
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

  def apply[T](fn: (T, Path) ⇒ Throwable | Unit): Save[T] = new Save[T] { def direct(t: T, dir: Path): Throwable | Unit = fn(t, dir) }
  def as[L, R: Save](fn: L ⇒ R): Save[L] = Save { (t, dir) ⇒ implicit val no = Atomic.no; fn(t).save(dir) }
}

trait Pri2
//  extends LowPrioritySave
{
  self: Save.type ⇒
//  implicit def narrow[U, T >: U](implicit save: Save[T], ev: U <:< T): Save[U] = ???
}

object Save
  extends LowPrioritySave
//extends Pri2
{

  sealed trait Atomic
  object Atomic {
    case object yes extends Atomic
    case object  no extends Atomic
  }

  implicit def withBasenameAsJSON[T](
    implicit
    basename: Basename[T],
    encoder: Encoder[T]
  ):
    Save[T] =
    Save {
      (t, dir) ⇒
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

  implicit def opt[T](implicit save: Save[T]): Save[Option[T]] =
    Save {
      case (Some(t), dir) ⇒ save(t, dir)(Atomic.no)
      case _ ⇒ Right(())
    }

  implicit class Ops[T](val t: T) extends AnyVal {
    def save(dir: Path)(implicit save: Save[T], atomic: Atomic = Atomic.yes): Throwable | Unit = save(t, dir)
  }

  trait syntax {
    @inline implicit def zarrSaveOps[T](t: T) = Ops(t)
  }
}
