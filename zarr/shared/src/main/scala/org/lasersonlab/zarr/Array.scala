package org.lasersonlab.zarr

import java.io.FileNotFoundException

import cats.{ Applicative, Eval, Traverse }
import hammerlab.option._
import hammerlab.path._
import hammerlab.shapeless.tlist._
import io.circe.Decoder
import org.lasersonlab.ndarray
import org.lasersonlab.ndarray.{ Arithmetic, Bytes, ScanRight, Sum, ToArray, TraverseIndices }
import DataType.read

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
    else {
      val bytes = compressor(path)
      Right(
        Chunk(
          bytes,
          shape,
          idx,
          start,
          end
        )
      )
    }
}

trait Key[T] {
  def apply(t: T): String
}

case class Array[T, Shape](
  metadata: Metadata[T, Shape],
  chunks: ndarray.Array.Aux[Chunk[T, Shape], Shape],
  attrs: Opt[Attrs] = None
)

object Array {

  implicit def toArray[
    T,
    Shape: Arithmetic.Id
  ]:
    ToArray.Aux[
      Array[T, Shape],
      T,
      Shape
    ] =
    ToArray[
      Array[T, Shape],
      T,
      Shape
    ](
      _.metadata.shape,
      (arr, idx) ⇒ {
        val chunkShape = arr.metadata.chunks

        import Arithmetic.Ops

        val chunkIdx = idx / chunkShape
        val  elemIdx = idx % chunkShape

        arr.chunks(chunkIdx)(elemIdx)
      }
    )

//  type ArrS[S] = ndarray.Array.Aux[?, S]
//  type ArrS[S] = λ[A ⇒ ndarray.Array.Aux[A, S]]

  implicitly[Traverse[List]].sequence

  implicit def traverse[Shape]: Traverse[λ[A ⇒ ndarray.Array.Aux[A, Shape]]] =
    new Traverse[λ[A ⇒ ndarray.Array.Aux[A, Shape]]] {
      type Arr[T] = ndarray.Array.Aux[T, Shape]
      override def traverse[G[_], A, B](fa: Arr[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[Arr[B]] = {
        foldRight()
        fa.map(f): Arr[G[B]]
        ???
      }

      override def foldLeft[A, B](fa: Arr[A], b: B)(f: (B, A) ⇒ B): B = ???

      override def foldRight[A, B](fa: Arr[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
    }

  def chunks[
     T : DataType.Aux,
     Shape: Arithmetic.Id,
  ](
    dir: Path,
    arrShape: Shape,
    chunkShape: Shape,
    compressor: Compressor
  )(
    implicit
    ti: Indices[Shape],
    ai: Arithmetic[Shape, Int],
    k: Key[Shape],
    scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
    sum: Sum.Aux[Shape, Int]
  ):
    Either[
      Exception,
      ndarray.Array.Aux[
        Chunk[T, Shape],
        Shape
      ]
    ] = {

    import Arithmetic.Ops

    // ops:
    // {+,-,*,/,min,%} (shape,int)

    val chunkRanges = (arrShape + chunkShape - 1) / chunkShape

//    val chunkRanges =
//      arrShape
//        .zip(chunkShape)
//        .map {
//          case (arrSize, chunkSize) ⇒
//            (arrSize + chunkSize - 1) / chunkSize
//        }

    import cats.implicits._

    //val trv = implicitly[Traverse[λ[A ⇒ ndarray.Array.Aux[A, Shape]]]]
    val trv: Traverse[λ[A ⇒ ndarray.Array.Aux[A, Shape]]] = traverse[Shape]

    val chunks =
      ti(chunkRanges)
        .map {
          idx ⇒
            val key = k(idx)

            val start = idx * chunkShape
            val end = arrShape min (idx * (chunkShape + 1))
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

//    val app: Applicative[Eith] = implicitly

    trv.sequence[Eith, Chunk[T, Shape]](chunks)

//    chunks.sequence[Eith, Chunk[T, Shape]]
  }

  def apply[
    T : DataType.Aux : Decoder,
    Shape : Arithmetic.Id : Indices : Key : Decoder
  ](
    dir: Path
  )(
    implicit
    d: Decoder[DataType.Aux[T]],
    ai: Arithmetic[Shape, Int],
    scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
    sum: Sum.Aux[Shape, Int]
  ):
    Either[Exception, Array[T, Shape]] = {
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
        chunks[T, Shape](
          dir,
          metadata.shape,
          metadata.chunks,
          metadata.compressor
        )
    } yield
      Array[T, Shape](
        metadata,
        chunks,
        attrs
      )
  }
}
