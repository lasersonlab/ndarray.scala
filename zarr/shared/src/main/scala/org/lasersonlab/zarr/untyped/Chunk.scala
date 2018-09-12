package org.lasersonlab.zarr.untyped

import java.io.FileNotFoundException
import java.nio.ByteBuffer

import cats.{ Eval, Foldable }
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.ndarray.{ Arithmetic, ScanRight, Sum }
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Compressor
import org.lasersonlab.zarr.dtype.DataType

/**
 * TODO: we only need the typed version of this
 *
 * A Zarr "chunk" file, represented as a [[Path]] that bytes are lazily loaded from
 *
 * TODO: parameterize Shape by its coordinate type (presumably [[Int]] or [[Long]]); replace [[arithmetic]], [[sum]] by
 * a "bi-fold"
 */
trait Chunk[Shape] {
  type T
  def path: Path
  def shape: Shape
  def idx: Shape
  def size: Int
  def strides: Shape
  def compressor: Compressor
  def sizeHint: Opt[Int]

  implicit def dtype: DataType.Aux[T]
  implicit def arithmetic: Arithmetic.Id[Shape]
  implicit def sum: Sum.Aux[Shape, Int]

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
      sum(idx * strides)
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

  type Aux[Shape, _T] = Chunk[Shape] { type T = _T }

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
    Aux[Shape, T]
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
        zarr.Chunk(
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

  implicit def foldable[Shape]: Foldable[Aux[Shape, ?]] =
    new Foldable[Aux[Shape, ?]] {
      type F[A] = Aux[Shape, A]
      def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  = fa.foldLeft ( b)(f)
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.foldRight(lb)(f)
    }
}
