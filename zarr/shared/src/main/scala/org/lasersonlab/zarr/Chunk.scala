package org.lasersonlab.zarr

import java.io.FileNotFoundException

import cats.{ Eval, Foldable }
import hammerlab.path._
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.io.Read
import org.lasersonlab.ndarray.{ Arithmetic, Bytes, ScanRight, Sum }
import org.lasersonlab.zarr.DataType.read

case class Chunk[
  T,
  _Shape
](
  override val bytes: Seq[Byte],
  override val shape: _Shape,
                 idx: _Shape,
               start: _Shape,
                 end: _Shape,
  override val  size: Int,
  override val  sizeProducts: _Shape
)(
  implicit
  dtype: DataType.Aux[T],
  val arithmetic: Arithmetic.Id[_Shape],
  scanRight: ScanRight.Aux[_Shape, Int, Int, _Shape],
  val sum: Sum.Aux[_Shape, Int]
)
extends Bytes[T] {
  type Shape = _Shape

  require(
    size * dtype.size <= bytes.length,
    s"Unexpected bytes size in chunk $idx (shape: $shape): ${bytes.length} instead of ${size * dtype.size} ($size * ${dtype.size})"
  )

  override implicit def read: Read[T] = DataType.read(dtype)
}

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
    else {
      val (size, sizeProducts) = scanRight(shape, 1, _ * _)
      Right(
        Chunk(
          compressor(path, size * dt.size),
          shape,
          idx,
          start,
          end,
          size,
          sizeProducts
        )
      )
    }
}
