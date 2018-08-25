package org.lasersonlab.zarr.group

import hammerlab.either._
import hammerlab.path._
import io.circe.Encoder
import org.hammerlab.str.Name

import scala.util.Try

case class Basename[T](override val toString: String)
object Basename {
  implicit def toString[T](basename: Basename[T]): String = basename.toString
}

trait Save[T] {
  def apply(t: T, dir: Path): Throwable | Unit
}
object Save {
  implicit def withBasenameAsJSON[T](
    implicit
    basename: Basename[T],
    encoder: Encoder[T]
  ):
    Save[T] =
    new Save[T] {
      def apply(t: T, dir: Path): Throwable | Unit =
        Try {
          (dir / basename)
            .write(
              encoder(t)
                .spaces2
            )
        }
        .toEither

    }
}
