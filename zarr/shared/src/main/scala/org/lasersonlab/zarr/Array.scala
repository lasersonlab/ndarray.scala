package org.lasersonlab.zarr

import java.io.FileNotFoundException

import cats.Traverse
import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder
import org.lasersonlab.ndarray.{ Arithmetic, Bytes, ScanRight, Sum }
import shapeless.Nat

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

object Array {

  def chunks[
    T,
    Shape: Arithmetic.Id,
    A[U]
  ](
           dir: Path,
      arrShape: Shape,
    chunkShape: Shape
  )(
    implicit
    ti: Indices.Aux[A, Shape],
    traverse: Traverse[A],
    ai: Arithmetic[Shape, Int],
    k: Key[Shape],
    scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
    sum: Sum.Aux[Shape, Int],
    compressor: Compressor,
    datatype: DataType.Aux[T]
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
    dt: Decoder[T],
  ):
    Either[
      Exception,
      Array[T, v.Shape, v.A, Bytes]
  ] = {
    import v._
    apply[T, v.Shape, v.A](dir)(
      // shouldn't have to do this? https://github.com/scala/bug/issues/11086
      d = d,
      ti = ti,
      traverse = traverse,
      ai = ai,
      scanRight = scanRight,
      sum = sum,
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
        chunks ← {
          implicit val _md = metadata
          import Metadata._
          chunks[T, Shape, A](
            dir,
            metadata.shape,
            metadata.chunks
          )
        }
      } yield
        new Array[T, Shape, A, Bytes](
          metadata,
          chunks,
          attrs
        )
  }
}
