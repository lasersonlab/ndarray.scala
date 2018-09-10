package org.lasersonlab.zarr

import java.io.FileNotFoundException
import java.nio.ByteBuffer

import cats.{ Eval, Foldable }
import hammerlab.path._
import org.lasersonlab.ndarray.{ Arithmetic, ScanRight, Sum }
import org.lasersonlab.zarr.dtype.DataType

case class Chunk[
  Shape,
  T
](
   path: Path,
  shape: Shape,
    idx: Shape,
  start: Shape,
    end: Shape,
   size: Int,
   sizeProducts: Shape,
   compressor: Compressor
)(
  sizeHint: Shape
)(
  implicit
  dtype: DataType.Aux[T],
  val arithmetic: Arithmetic.Id[Shape],
  scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
  val sum: Sum.Aux[Shape, Int]
)
{
  lazy val bytes = {
    val chunkSize = scanRight(sizeHint, 1, _ * _)._1 * dtype.size
    val bytes = compressor(path, chunkSize)
    require(
      chunkSize == bytes.length,
      s"Expected $chunkSize bytes in chunk $idx (shape: $shape, sizeHint $sizeHint, dtype $dtype), found ${bytes.length}"
    )
    bytes
  }

  lazy val buff = ByteBuffer.wrap(bytes)

  @inline def apply(idx: Int): T = dtype.read(buff, idx)

  def apply(idx: Shape): T =
    dtype.read(
      buff,
      sum(
        idx * sizeProducts
      )
    )

  def foldLeft[V](base: V)(fn: (V, T) ⇒ V): V = {
    buff.reset()
    var v = base
    var i = 0
    while (i < size) {
      v = fn(v, dtype(buff))
      i += 1
    }
    v
  }

  def foldRight[V](base: V)(fn: (T, V) ⇒ V): V = {
    buff.reset()
    var v = base
    var i = size - 1
    while (i >= 0) {
      v = fn(dtype(buff), v)
      i -= 1
    }
    v
  }
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
    sizeHint: Shape
  )(
    implicit
    dt: DataType.Aux[T],
    scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
    sum: Sum.Aux[Shape, Int]
  ):
    Exception |
    Chunk[Shape, T]
  =
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
          path,
          shape,
          idx,
          start,
          end,
          size,
          sizeProducts,
          compressor
        )(
          sizeHint
        )
      )
    }

  implicit def foldable[Shape]: Foldable[Chunk[Shape, ?]] =
    new Foldable[Chunk[Shape, ?]] {
      type F[A] = Chunk[Shape, A]
      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B = fa.foldLeft(b)(f)
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.foldRight(lb)(f)
    }
}
