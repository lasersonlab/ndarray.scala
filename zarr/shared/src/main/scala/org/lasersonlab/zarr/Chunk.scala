package org.lasersonlab.zarr

import java.io.FileNotFoundException

import hammerlab.path._
import org.lasersonlab.ndarray.{ Arithmetic, Bytes, ScanRight, Sum }
import org.lasersonlab.zarr.DataType.read

case class Chunk[
  T,
  Shape: Arithmetic.Id
](
  override val bytes: scala.Array[Byte],
  override val shape: Shape,
                 idx: Shape,
               start: Shape,
                 end: Shape
)(
  implicit
  dtype: DataType.Aux[T],
  scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
  sum: Sum.Aux[Shape, Int]
)
extends Bytes[T, Shape](bytes, shape)

object Chunk {
  def apply[
    T,
    Shape: Arithmetic.Id
  ](
          path: Path,
         shape: Shape,
           idx: Shape,
         start: Shape,
           end: Shape,
    compressor: Compressor
  )(
    implicit
    dt: DataType.Aux[T],
    scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
    sum: Sum.Aux[Shape, Int]
  ):
    Either[
      Exception,
      Chunk[T, Shape]
    ] =
    if (!path.exists)
      Left(
        new FileNotFoundException(
          path.toString
        )
      )
    else
      Right(
        Chunk(
          compressor(path),
          shape,
          idx,
          start,
          end
        )
      )
}
