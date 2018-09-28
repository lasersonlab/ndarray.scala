package org.lasersonlab

import _root_.cats.Traverse
import _root_.cats.implicits._
import io.circe.Decoder.Result
import io.circe.generic.AutoDerivation
import io.circe.{ Decoder, DecodingFailure, HCursor, Parser, ParsingFailure, Printer }
import org.hammerlab.paths.HasPathOps
import org.lasersonlab.circe.DecoderK
import org.lasersonlab.ndarray.Arithmetic
import org.lasersonlab.shapeless.Zip
import org.lasersonlab.zarr.io.{ Load, Save }
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.zarr.utils.Idx.Long.CastException
import org.lasersonlab.zarr.utils.opt.OptCodec

import scala.util.Try

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
     with utils.slist.Codecs
     with HasPathOps
     with hammerlab.either
     with hammerlab.math.utils
     with Idx.syntax
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

  /**
   * Total size and chunk-size of a given dimension of an [[Array]]
   *
   * The [[size total size]] type is [[Idx parameterizable]], while [[chunk chunk-sizes]] must always be
   * [[Chunk.Idx ints]]
   *
   * // TODO: move to own file
   */
  case class Dimension[Idx](
    size: Idx,
    chunk: Chunk.Idx,
    range: Chunk.Idx
  )
  object Dimension {
    def apply(arr: Chunk.Idx): Dimension[Chunk.Idx] = Dimension(arr, arr, 1)
    def int(
      arr: Int,
      chunk: Chunk.Idx
    ):
      Dimension[Int] =
      Dimension(
        arr,
        chunk,
        (arr + chunk - 1) / chunk
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
    import Zip.Ops
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
                           import Idx.helpers.specify
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

  trait api {
    type Idx
    import org.lasersonlab.{ zarr ⇒ z }
    type Group = z.Group[Idx]
    object Group {
      type Metadata = z.Group.Metadata
    }
    type Array[Shape[_], T] = z.Array.Of[Shape, Idx, T]
    object Array {
      type Metadata[Shape[_], T] = z.Metadata[Shape, Idx, T]
    }
  }
  trait int
    extends api {
    type Idx = Int
  }
  trait long
    extends api {
    type Idx = Long
  }

  type Metadata[Shape[_], Idx, T] = array.metadata.Metadata[Shape, Idx, T]
  val Metadata = array.metadata.Metadata

  val pprint = Printer.spaces4.copy(colonLeft = "").pretty _
}
