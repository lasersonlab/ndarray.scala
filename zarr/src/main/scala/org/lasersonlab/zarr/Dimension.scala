package org.lasersonlab.zarr

import cats.Traverse
import circe._, Decoder.Result
import org.lasersonlab.circe.DecoderK
import org.lasersonlab.shapeless.Zip
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.zarr.utils.Idx.Long.CastException

import scala.util.Try

/**
 * Total size and chunk-size of a given dimension of an [[Array]]
 *
 * The [[size total size]] type is [[Idx parameterizable]], while [[chunk chunk-sizes]] must always be
 * [[Chunk.Idx ints]]
 */
case class Dimension[Idx](
  size: Idx,
  chunk: Chunk.Idx,
  range: Chunk.Idx
)
object Dimension {
  def apply(arr: Chunk.Idx): Dimension[Chunk.Idx] = Dimension(arr, arr, 1)
  // TODO: make this another `apply` overload?
  def int(
    arr: Int,
    chunk: Chunk.Idx
  ):
    Dimension[Int] =
    Dimension(
      arr,
      chunk,
      arr /↑ chunk
    )

  def apply[Idx](
    arr: Idx,
    chunk: Chunk.Idx
  )(
    implicit
    idx: utils.Idx.T[Idx]
  ):
    CastException | Dimension[Idx]
  = {
    import idx._
    idx.int {
      (arr + chunk - 1) / chunk
    }
    .map {
      Dimension(arr, chunk, _)
    }
  }
}
object Dimensions {
  implicit def decodeList[
    Shape[_]
    : DecoderK
    : Zip
    : Traverse
  ](
    implicit
    idx: Idx
  ):
    Decoder[
      Shape[
        Dimension[
          idx.T
        ]
      ]
    ] =
    new Decoder[Shape[Dimension[idx.T]]] {
      override def apply(c: HCursor): Result[Shape[Dimension[idx.T]]] =
        for {
               shape ← c.downField( "shape").as[Shape[idx.T]]
              chunks ← c.downField("chunks").as[Shape[Chunk.Idx]]
          dimensions ←
                 Try {
                   shape.zip(chunks)
                 }
                 .toEither
                 .flatMap {
                   _
                     .map {
                       case (shape, chunk) ⇒
                         Dimension(shape, chunk)
                     }
                     .sequence
                 }
                 .left
                 .map {
                   DecodingFailure.fromThrowable(
                     _,
                     c.history
                   )
                 }
        } yield
          dimensions
    }
}

