package org.lasersonlab.zarr

import java.nio.ByteBuffer

import cats.{ Eval, FlatMap, Foldable, Functor, Semigroupal }
import cats.implicits._
import hammerlab.option._
import org.lasersonlab.io.FileNotFoundException
import org.lasersonlab.ndarray.ArrayLike
import org.lasersonlab.slist.{ Scannable, Zip }
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.Chunk.Idx

import scala.concurrent.ExecutionContext

/**
 * A Zarr "chunk" file, represented as a [[Path]] that bytes are lazily loaded from
 *
 * @param bytes path to chunk file
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
  ShapeT[_]
  : Foldable
  : Zip,
  T
](
       bytes:       Arr[Byte],
       shape:     ShapeT[Idx],
         idx:     ShapeT[Idx],
        size:        Int     ,
     strides:     ShapeT[Idx],
  compressor: Compressor     ,
    sizeHint:        Opt[Int]
)(
  implicit
  val dtype: DataType[T]
) {

  type Shape = ShapeT[Idx]

//  println(s"Got chunk with ${bytes.length} bytes, shape $shape, idx $idx, size $size")
//  lazy val bytes =
//    compressor(path, size * dtype.size)
//      .map {
//        bytes ⇒
//          sizeHint
//            .fold {
//              require(
//                size * dtype.size <= bytes.length,
//                s"$path: expected at least ${size * dtype.size} bytes in chunk $idx ($shape = $size records of type $dtype, size ${dtype.size}), found ${bytes.length}"
//              )
//            } {
//              expected ⇒
//                require(
//                  expected == bytes.length,
//                  s"$path: expected $expected bytes in chunk $idx ($shape = $size records of type $dtype, size ${dtype.size}), found ${bytes.length}"
//                )
//            }
//        bytes
//      }

  lazy val buff = ByteBuffer.wrap(bytes)

  @inline def apply(idx: Int): T = dtype.read(buff, idx)

  def apply(idx: Shape): T =
    dtype
      .read(
        buff,
        idx
          .zip(strides)
          .foldLeft(0) {
            case (sum, (idx,  stride)) ⇒
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
    ShapeT[_]
            : Foldable
            : Scannable
            : Zip,
         T
            : DataType
  ](
          path: Path,
         shape: ShapeT[Idx],
           idx: ShapeT[Idx],
    compressor: Compressor,
      sizeHint: Opt[Int] = None
  )(
    implicit ec: ExecutionContext
  ):
    F[Chunk[ShapeT, T]]
  =
    path
      .exists
      .flatMap {
        exists ⇒
          if (!exists)
            throw FileNotFoundException(path)
          else {
            val (size, sizeProducts) = shape.scanRight(1) { _ * _ }
            compressor(path, sizeHint)
              .map {
                Chunk(
                  _,
                  shape,
                  idx,
                  size,
                  sizeProducts,
                  compressor,
                  sizeHint
                )
              }
          }
      }

  implicit def arrayLike[S[_]]: ArrayLike.Aux[Chunk[S, ?], S] =
    new ArrayLike[Chunk[S, ?]] {
      type Shape[U] = S[U]
      @inline def shape   (chunk: Chunk[Shape, _]): Shape[Int] = chunk.shape
      @inline def apply[T](chunk: Chunk[Shape, T], idx: S[Int]): T = chunk(idx)
    }

  implicit def foldable[Shape[_]]: Foldable[Chunk[Shape, ?]] =
    new Foldable[Chunk[Shape, ?]] {
      type F[A] = Chunk[Shape, A]
      def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  = fa.foldLeft ( b)(f)
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.foldRight(lb)(f)
    }
}
