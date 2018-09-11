package org.lasersonlab.zarr

import java.io.FileNotFoundException
import java.nio.ByteBuffer

import cats.{ Eval, Foldable }
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.ndarray.{ Arithmetic, ScanRight, Sum }
import org.lasersonlab.zarr.dtype.DataType

/**
 * A Zarr "chunk" file, represented as a [[Path]] that bytes are lazily loaded from
 *
 * TODO: parameterize Shape by its coordinate type (presumably [[Int]] or [[Long]]); replace [[arithmetic]], [[sum]] by
 * a "bi-fold"
 *
 * @param path path to chunk file
 * @param shape shape of this chunk
 * @param idx index of this chunk inside larger Zarr [[Array]]
 * @param size number of elements in this chunk (product of elements of shape)
 * @param strides number of elements in a flattened/linearized array that each component of [[shape]] corresponds to;
 *                has same number of elements as [[shape]], last element is 1, and other elements are the result of a
 *                scanRight-product from there
 * @param arithmetic evidence for dot-product-ing [[Shape]]s
 * @param sum evidence for summing each dimensions' stride-weighted component to find a 1-D address of a given N-D index
 * @tparam Shape index/shape used for addressing individual elements in this chunk (and also the larger Zarr [[Array]])
 * @tparam T element type
 */
case class Chunk[
  Shape,
  _T
](
  path: Path,
  shape: Shape,
  idx: Shape,
  size: Int,
  strides: Shape,
  compressor: Compressor,
  sizeHint: Opt[Int]
)(
  implicit
  val dtype: DataType.Aux[_T],
  val arithmetic: Arithmetic.Id[Shape],
  val sum: Sum.Aux[Shape, Int]
)
extends untyped.Chunk[Shape] {
  type T = _T
}

object Chunk {

  /**
   * Convenience-constructor for a chunk of a Zarr [[Array]]
   *
   * Does some error-checking, and computes the per-dimension strides and total size of the chunk
   */
  def apply[
    T,
    Shape: Arithmetic.Id
  ](
          path: Path,
         shape: Shape,
           idx: Shape,
    compressor: Compressor,
      sizeHint: Opt[Int] = None
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
          size,
          sizeProducts,
          compressor,
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
