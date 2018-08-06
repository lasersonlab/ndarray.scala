package org.lasersonlab.zarr

import java.io.FileNotFoundException

import cats.Traverse
import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder
import org.lasersonlab.ndarray
import org.lasersonlab.ndarray.{ Arithmetic, Bytes, ScanRight, Sum, ToArray }
import org.lasersonlab.zarr.DataType.read

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
    else
      Right(
        Chunk(
          compressor(path),
          shape,
          idx,
          start,
          end
        )
      )
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

  trait Rows[In, Rows[_]] {
    type Row
    def apply(in: In): Rows[Row]
    def apply(in: In, idx: Int): Row
  }
  object Rows {
    type Aux[In, Rs[_], _Row] =
      Rows[In, Rs] {
        type Row = _Row
      }
  }

  import ndarray.Array.Aux

  def chunks[
    T : DataType.Aux,
    Shape: Arithmetic.Id,
    A[U] <: Aux[U, Shape]
  ](
           dir: Path,
      arrShape: Shape,
    chunkShape: Shape,
    compressor: Compressor
  )(
    implicit
    ti: Indices[Shape, A[Shape]],
    traverse: Traverse[A],
    ai: Arithmetic[Shape, Int],
    k: Key[Shape],
    scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
    sum: Sum.Aux[Shape, Int]
  ):
    Either[
      Exception,
      A[
        Chunk[T, Shape]
      ]
    ] = {

    import Arithmetic.Ops

    val chunkRanges = (arrShape + chunkShape - 1) / chunkShape

    // Either Traverse, Either Applicative, Functor syntax
    import cats.implicits._

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

    traverse.sequence[Eith, Chunk[T, Shape]](chunks)

//    val app: Applicative[Eith] = implicitly
//    chunks.sequence[Eith, Chunk[T, Shape]]
  }

  def apply[
    T : DataType.Aux : Decoder,
    Shape : Arithmetic.Id : Key : Decoder,
    A[U] <: Aux[U, Shape]
  ](
    dir: Path
  )(
    implicit
    d: Decoder[DataType.Aux[T]],
    ti: Indices[Shape, A[Shape]],
    traverse: Traverse[A],
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
          chunks[T, Shape, A](
            dir,
            metadata.shape,
            metadata.chunks,
            metadata.compressor
          )
      } yield
        new Array[T, Shape](
          metadata,
          chunks,
          attrs
        )
  }
}
