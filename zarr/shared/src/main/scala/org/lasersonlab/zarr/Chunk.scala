package org.lasersonlab.zarr

import java.io.FileNotFoundException
import java.nio.ByteBuffer

import cats.{ Eval, Foldable }
import hammerlab.path._
import org.lasersonlab.ndarray.io.Read
import org.lasersonlab.ndarray.{ Arithmetic, ArrayLike, ScanRight, Sum }
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

  implicit val read: Read[T] = DataType.read(dtype)

  lazy val buff = ByteBuffer.wrap(bytes)

  @inline def apply(idx: Int): T = read(buff, idx)

  def apply(idx: Shape): T =
    read(
      buff,
      sum(
        idx * sizeProducts
      )
    )
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
      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B = {
        var ret = b
        var idx = 0
        while (idx < fa.size) {
          ret = f(ret, fa(idx))
          idx += 1
        }
        ret
      }
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
    }

  implicit def arrayLike[S]: ArrayLike.Aux[Chunk[S, ?], S] =
    new ArrayLike[Chunk[S, ?]] {
      type Shape = S
      @inline def shape(a: Chunk[S, _]): Shape = a.shape
      @inline def apply[T](a: Chunk[Shape, T], idx: Shape): T = a(idx)
    }
}
