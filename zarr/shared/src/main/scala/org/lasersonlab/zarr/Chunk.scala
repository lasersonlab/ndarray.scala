package org.lasersonlab.zarr

import java.io.FileNotFoundException
import java.nio.ByteBuffer

import cats.{ Eval, Foldable, Semigroupal }
import cats.implicits._
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.ndarray.Scannable
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.Chunk.Idx

/**
 * A Zarr "chunk" file, represented as a [[Path]] that bytes are lazily loaded from
 *
 * @param path path to chunk file
 * @param shape shape of this chunk
 * @param idx index of this chunk inside larger Zarr [[Array]]
 * @param size number of elements in this chunk (product of elements of shape)
 * @param strides number of elements in a flattened/linearized array that each component of [[shape]] corresponds to;
 *                has same number of elements as [[shape]], last element is 1, and other elements are the result of a
 *                scanRight-product from there
 * @tparam ShapeT index/shape used for addressing individual elements in this chunk (and also the larger Zarr [[Array]])
 * @tparam T element type
 */
case class Chunk[
  ShapeT[_],
  T
](
  path: Path,
  shape: ShapeT[Idx],
  idx: ShapeT[Idx],
  size: Int,
  strides: ShapeT[Idx],
  compressor: Compressor,
  sizeHint: Opt[Int]
)(
  implicit
  val dtype: DataType.Aux[T],
  val product: Semigroupal[ShapeT],
  val shapeFoldable: Foldable[ShapeT]
) {

  type Shape = ShapeT[Idx]

  lazy val bytes = {
    val bytes = compressor(path, size * dtype.size)
    sizeHint
    .fold {
      require(
        size * dtype.size <= bytes.length,
        s"Expected at least ${size * dtype.size} bytes in chunk $idx ($shape = $size records of type $dtype, size ${dtype.size}), found ${bytes.length}"
      )
    } {
      expected ⇒
        require(
          expected == bytes.length,
          s"Expected $expected bytes in chunk $idx ($shape = $size records of type $dtype, size ${dtype.size}), found ${bytes.length}"
        )
    }
    bytes
  }

  lazy val buff = ByteBuffer.wrap(bytes)

  @inline def apply(idx: Int): T = dtype.read(buff, idx)

  def apply(idx: Shape): T =
    dtype.read(
      buff,
      idx
        .product(strides)
        .foldLeft(0) {
          case (sum, (idx, stride)) ⇒
            sum + idx * stride
        }
    )

  def foldLeft[V](base: V)(fn: (V, T) ⇒ V): V = {
    buff.clear()
    var v = base
    var i = 0
    while (i < size) {
      v = fn(v, dtype(buff))
      i += 1
    }
    v
  }

  def foldRight[V](base: V)(fn: (T, V) ⇒ V): V = {
    buff.clear()
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

  type Idx = Int

  /**
   * Convenience-constructor for a chunk of a Zarr [[Array]]
   *
   * Does some error-checking, and computes the per-dimension strides and total size of the chunk
   */
  def apply[
    ShapeT[_]: Foldable : Scannable,
    T
  ](
          path: Path,
         shape: ShapeT[Idx],
           idx: ShapeT[Idx],
    compressor: Compressor,
      sizeHint: Opt[Int] = None
  )(
    implicit
    dt: DataType.Aux[T],
    product: Semigroupal[ShapeT]
  ):
    Exception |
    Chunk[ShapeT, T]
  =
    if (!path.exists)
      Left(
        new FileNotFoundException(
          path.toString
        )
      )
    else {
      import Scannable.Ops
      val (size, sizeProducts) = shape.scanRight(1) { _ * _ }
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

  implicit def foldable[Shape[_]]: Foldable[Chunk[Shape, ?]] =
    new Foldable[Chunk[Shape, ?]] {
      type F[A] = Chunk[Shape, A]
      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B = fa.foldLeft(b)(f)
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.foldRight(lb)(f)
    }
}
