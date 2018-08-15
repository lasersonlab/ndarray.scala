package org.lasersonlab.zarr

import java.io.FileNotFoundException

import cats.{ Eval, Foldable }
import hammerlab.path._
import hammerlab.shapeless
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.{ Arithmetic, Bytes, ScanRight, Sum }
import org.lasersonlab.zarr.DataType.read

case class Chunk[
  T,
  Shape: Arithmetic.Id
](
  override val bytes: Seq[Byte],
  override val shape: Shape,
                 idx: Shape,
               start: Shape,
                 end: Shape,
  override val  size: Int,
  override val  sizeProducts: Shape
)(
  implicit
  dtype: DataType.Aux[T],
  scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
  sum: Sum.Aux[Shape, Int]
)
extends Bytes[T, Shape](bytes, shape, size, sizeProducts) {
  require(
    size * dtype.size <= bytes.length,
    s"Unexpected bytes size in chunk $idx (shape: $shape): ${bytes.length} instead of ${size * dtype.size} ($size * ${dtype.size})"
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

  implicit def consFoldable[
    TL <: TList.Aux[Int]
  ]:
    Foldable[
      Chunk[?, TL]
    ] = {
    type Shape = TL
    new Foldable[Chunk[?, Shape]] {
      type F[A] = Chunk[A, Shape]
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
  }
}

trait TraverseIndices[TL <: TList] {
  def apply(tl: TL): Iterator[TL]
}
object TraverseIndices {
  implicit val tnil: TraverseIndices[TNil] =
    new TraverseIndices[TNil] {
      override def apply(tl: TNil): Iterator[TNil] = Iterator(TNil)
    }

  implicit def cons[TL <: TList](
    implicit
    ti: TraverseIndices[TL],
    pp: Prepend[Int, TL]
  ):
    TraverseIndices[Int :: TL] =
    new TraverseIndices[Int :: TL] {
      def apply(tl: Int :: TL): Iterator[Int :: TL] =
        tl match {
          case h :: t ⇒
            for {
              r ← (0 until h).iterator
              rest ← ti(t)
            } yield
              r :: rest
        }
    }
}
