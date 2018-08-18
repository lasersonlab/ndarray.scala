package org.lasersonlab.zarr

import java.io.FileNotFoundException

import cats.Traverse
import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder
import org.lasersonlab.ndarray.{ Arithmetic, Bytes, ScanRight, Sum }
import org.lasersonlab.zarr.dtype.DataType
import shapeless.Nat

/**
 * A Zarr N-dimensional array
 *
 * Storage of the ND-array of chunks, as well as the records in each chunk, are each a configurable type-param; see
 * companion-object for some convenient constructors
 */
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

  /**
   * Load an ND-array of chunks from a [[Path directory]]
   *
   * Each chunk lives in a file with basename given by the provided [[Key]] ('.'-joined indices)
   */
  def chunks[
        T: DataType.Aux,
    Shape: Arithmetic.Id,
     A[U]: Traverse
  ](
           dir: Path,
      arrShape: Shape,
    chunkShape: Shape
  )(
     implicit
     indices: Indices.Aux[A, Shape],
     ai: Arithmetic[Shape, Int],
     key: Key[Shape],
     scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
     sum: Sum.Aux[Shape, Int],
     compressor: Compressor,
  ):
    Exception |
    A[Bytes[T]]
  = {

    val chunkRanges = (arrShape + chunkShape - 1) / chunkShape

    // We use Traverse and Applicative instances for Either, Traverse[A], and Functor syntax
    import cats.implicits._

    val chunks =
      indices(chunkRanges)
        .map {
            idx ⇒
              val start = idx * chunkShape
              val end = arrShape min ((idx + 1) * chunkShape)

              // chunks in the last "row" of any dimension may be smaller
              val shape = end - start

              Chunk(
                dir / key(idx),
                shape,
                idx,
                start,
                end,
                compressor
              )
          }

    // A[Err | Chunk] -> Err | A[Chunk]
    chunks
      .sequence[
        Exception | ?,
        Bytes[T]
      ]
  }

  /**
   * Convenience-constructor: given a data-type and a [[Nat (type-level) number of dimensions]], load an [[Array]] from
   * a [[Path directory]]
   *
   * Uses a [[VectorInts]] as evidence for mapping from the [[Nat]] to a concrete shape
   */
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
    Exception |
    Array[T, v.Shape, v.A, Bytes]
  = {
    import v._
    apply[T, v.Shape, v.A](dir)(
      // shouldn't have to do list all these explicitly: https://github.com/scala/bug/issues/11086
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
    Exception |
    Array[T, Shape, A, Bytes]
  = {
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
