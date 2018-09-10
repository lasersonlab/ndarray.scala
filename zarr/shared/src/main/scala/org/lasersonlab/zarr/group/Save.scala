package org.lasersonlab.zarr.group

import hammerlab.either._
import hammerlab.option._
import hammerlab.path._
import hammerlab.str._
import io.circe.{ Encoder, Printer }

import scala.util.Try

case class Basename[T](override val toString: String)
object Basename {
  implicit def toString[T](basename: Basename[T]): String = basename.toString
  implicit def toStr   [T](basename: Basename[T]): Str    = basename.toString
}

trait Save[T] {
  def apply(t: T, dir: Path): Throwable | Unit
}
object Save {
  val print = Printer.spaces4.copy(colonLeft = "").pretty _
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
              print(
                encoder(t)
              )
            )
        }
        .toEither

    }

  implicit def opt[T](implicit save: Save[T]): Save[Opt[T]] =
    new Save[Opt[T]] {
      def apply(t: Opt[T], dir: Path): Throwable | Unit =
        t match {
          case Som(t) ⇒ save(t, dir)
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
