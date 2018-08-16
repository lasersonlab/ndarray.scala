package org.lasersonlab.zarr

import java.io.FileNotFoundException

import cats.Traverse
import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder
import org.lasersonlab.ndarray.{ Arithmetic, Bytes, ScanRight, Sum }

case class Array[T, Shape, A[_], Chunk[_]](
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
      A[
        Bytes[T]
      ]
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
    T: DataType.Aux : Decoder,
    Shape: Arithmetic.Id : Key : Decoder,
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
