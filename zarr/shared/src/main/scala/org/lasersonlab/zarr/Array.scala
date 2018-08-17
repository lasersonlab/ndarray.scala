package org.lasersonlab.zarr

import java.io.FileNotFoundException

import cats.{ Applicative, Eval, Traverse }
import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.Vectors._
import org.lasersonlab.ndarray.{ Arithmetic, Bytes, ScanRight, Sum }
import org.lasersonlab.zarr.Ints._
import shapeless.Nat

trait ArrayI[T] {
  type Shape
  type A[_]
  type Chunk[_]

  implicit def traverseA: Traverse[A]
  implicit def traverseChunk: Traverse[Chunk]

  def metadata: Metadata[T, Shape]
  def chunks: A[Chunk[T]]
  def attrs: Opt[Attrs] = None
}

object ArrayI {
  implicit val traverse: Traverse[ArrayI] =
    new Traverse[ArrayI] {
      def traverse[G[_], A, B](fa: ArrayI[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[ArrayI[B]] = ???
      def foldLeft [A, B](fa: ArrayI[A], b: B)(f: (B, A) ⇒ B): B = ???
      def foldRight[A, B](fa: ArrayI[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
    }
}

trait VectorInts[N <: Nat] {
  type Shape
  type A[_]

  implicit def arithmetic: Arithmetic.Id[Shape]
  implicit def ds: Decoder[Shape]
  implicit def ti: Indices.Aux[A, Shape]
  implicit def traverse: Traverse[A]
  implicit def ai: Arithmetic[Shape, Int]
  implicit def key: Key[Shape]
  implicit def scanRight: ScanRight.Aux[Shape, Int, Int, Shape]
  implicit def sum: Sum.Aux[Shape, Int]
}
object VectorInts {
  type Aux[N <: Nat, _S, _A[_]] =
    VectorInts[N] {
      type Shape = _S;
      type A[U] = _A[U]
    }

  def make[N <: Nat, _S, _A[_]](
    implicit
    _arithmetic: Arithmetic.Id[_S],
    _ds: Decoder[_S],
    _key: Key[_S],
    _ti: Indices.Aux[_A, _S],
    _traverse: Traverse[_A],
    _ai: Arithmetic[_S, Int],
    _scanRight: ScanRight.Aux[_S, Int, Int, _S],
    _sum: Sum.Aux[_S, Int]
  ):
    Aux[N, _S, _A] =
    new VectorInts[N] {
      type Shape = _S
      type A[U] = _A[U]

      implicit val arithmetic = _arithmetic
      implicit val ds = _ds
      implicit val key = _key
      implicit val ti = _ti
      implicit val traverse = _traverse
      implicit val ai = _ai
      implicit val scanRight = _scanRight
      implicit val sum = _sum
    }

  import shapeless.nat._
  import cats.implicits._

  implicit val `1` = make[_1, Ints1, Vector1]
  implicit val `2` = make[_2, Ints2, Vector2]
  implicit val `3` = make[_3, Ints3, Vector3]
  implicit val `4` = make[_4, Ints4, Vector4]
  implicit val `5` = make[_5, Ints5, Vector5]
  implicit val `6` = make[_6, Ints6, Vector6]
}

case class Array[
  T,
  Shape,
  A[_],
  Chunk[_]
](
  metadata: Metadata[T, Shape],
  chunks: A[Chunk[T]],
  attrs: Opt[Attrs] = None
)

trait Index[A[_]] {
  type Idx
  def apply[T](a: A[T], idx: Idx): T
}
object Index {
  type Aux[A[_], _Idx] = Index[A] { type Idx = _Idx }
}

object Array {

  def chunks[
    T : DataType.Aux,
    Shape: Arithmetic.Id,
    A[U]
  ](
           dir: Path,
      arrShape: Shape,
    chunkShape: Shape,
    compressor: Compressor
  )(
    implicit
    ti: Indices.Aux[A, Shape],
    traverse: Traverse[A],
    ai: Arithmetic[Shape, Int],
    k: Key[Shape],
    scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
    sum: Sum.Aux[Shape, Int]
  ):
    Either[
      Exception,
      A[Bytes[T]]
    ] = {

    import Arithmetic.Ops

    val chunkRanges = (arrShape + chunkShape - 1) / chunkShape

    // We use Traverse and Applicative instances for Either, and Functor syntax
    import cats.implicits._

    val chunks =
      ti(chunkRanges)
        .map {
          idx ⇒
            val key = k(idx)
            val start = idx * chunkShape
            val end = arrShape min ((idx + 1) * chunkShape)
            val shape = end - start
            Chunk(
              dir / key,
              shape,
              idx,
              start,
              end,
              compressor
            )
        }

    type Eith[U] = Either[Exception, U]

    chunks.sequence[Eith, Bytes[T]]
  }

  def apply[
    T,
    N <: Nat
  ](
    dir: Path
  )(
    implicit
    v: VectorInts[N],
    d: Decoder[DataType.Aux[T]],
    dd: DataType.Aux[T],
    dt: Decoder[T],
  ):
    Either[
      Exception,
      Array[T, v.Shape, v.A, Bytes]
  ] = {
    import v._
    implicitly[Indices.Aux[v.A, v.Shape]](ti)
    apply[T, v.Shape, v.A](dir)(
      // shouldn't have to do this; https://github.com/scala/bug/issues/11086
      d = d,
      ti = ti,
      traverse = traverse,
      ai = ai,
      scanRight = scanRight,
      sum = sum,
      dd = dd,
      dt = dt,
      arithmetic = arithmetic,
      key = key,
      ds = ds
    )
  }

  def apply[
    T,
    Shape,
    A[U]
  ](
    dir: Path
  )(
    implicit
    d: Decoder[DataType.Aux[T]],
    ti: Indices.Aux[A, Shape],
    traverse: Traverse[A],
    ai: Arithmetic[Shape, Int],
    scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
    sum: Sum.Aux[Shape, Int],
    dd: DataType.Aux[T],
    dt: Decoder[T],
    arithmetic: Arithmetic.Id[Shape],
    key: Key[Shape],
    ds: Decoder[Shape],
  ):
    Either[Exception, Array[T, Shape, A, Bytes]] = {
    if (!dir.exists)
      Left(
        new FileNotFoundException(
          dir.toString
        )
      )
    else
      for {
        metadata ← Metadata[T, Shape](dir)
        attrs ← Attrs(dir)
        chunks ←
          chunks[T, Shape, A](
            dir,
            metadata.shape,
            metadata.chunks,
            metadata.compressor
          )
      } yield
        new Array[T, Shape, A, Bytes](
          metadata,
          chunks,
          attrs
        )
  }
}
