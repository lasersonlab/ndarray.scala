package org.lasersonlab

import cats.{ Functor, Semigroupal }
import cats.implicits._
import io.circe.{ Parser, ParsingFailure }
import io.circe.generic.AutoDerivation
import org.hammerlab.paths.HasPathOps
import org.lasersonlab.ndarray.Arithmetic
import org.lasersonlab.zarr.io.{ Load, Save }
import org.lasersonlab.zarr.utils.opt.OptCodec
import org.lasersonlab.zarr.utils.tlist.TListCodec

/**
 * Spec / Format questions:
 *
 * Is a .zgroup entry really needed in every "directory" above an array? Doesn't that conflict with treating the paths
 * as ≈opaque blobs and not requiring filesystem-semantics in a backing store?
 *
 * Is "" an allowed fill_value for a fixed-length string datatype, e.g. "|S12"? See question in
 * [[zarr.FillValue.Decoder.string]]'s docs
 *
 * When is `fill_value` used? Are there sparseness facilities baked in to Zarr? Aren't chunks read in always of size
 * ${datatype.size} * $num_records?
 *
 * Does the reference implementation write extremal/boundary chunks as smaller, or compress a them as a buffer the size
 * of full-sized/interior chunks? Seems like the latter… some edges to check here around handling that as well.
 */
package object zarr
  extends Arithmetic.HasOps
     with OptCodec
     with TListCodec
     with utils.slist.Codecs
     with HasPathOps
     with hammerlab.either
     with hammerlab.math.utils
     with Load.syntax
     with Save.syntax {

  /**
   * inject a bunch of circe aliases in the package here, otherwise its top-level package `io` conflicts with an
   * eponymous [[zarr]] sub-package
   */
  object circe {
    import _root_.io.{ circe ⇒ c }

    def encode[T](t: T)(implicit e: Encoder[T]): Json = e(t)

    object auto extends AutoDerivation
    object parser extends Parser {
      @inline def parse(input: String): Either[ParsingFailure, Json] = c.parser.parse(input)
    }

    val         Encoder = c.        Encoder
    val         Decoder = c.        Decoder
    val            Json = c.           Json
    val DecodingFailure = c.DecodingFailure

    type            Json    = c.           Json
    type         Encoder[T] = c.        Encoder[T]
    type         Decoder[T] = c.        Decoder[T]
    type DecodingFailure    = c.DecodingFailure
    type         HCursor    = c.        HCursor
  }

  case class Dimension[Idx](arr: Idx, chunk: Chunk.Idx)
  object Dimension {
    def apply(arr: Chunk.Idx): Dimension[Chunk.Idx] = Dimension(arr, arr)
  }
  object Dimensions {
    def apply[
      Shape[_]: Semigroupal : Functor,
      Idx
    ](
      arr: Shape[Idx],
      chunks: Shape[Chunk.Idx]
    ):
      Shape[Dimension[Idx]] =
      arr
        .product(chunks)
        .map {
          case (arr, chunk) ⇒
            Dimension(arr, chunk)
        }
  }
}
