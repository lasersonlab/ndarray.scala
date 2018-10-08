package org.lasersonlab

import io.circe.generic.AutoDerivation
import io.circe.{ Parser, ParsingFailure, Printer }
import org.hammerlab.paths.HasPathOps
import org.lasersonlab.ndarray.Arithmetic
import org.lasersonlab.shapeless.{ Scannable, Zip }
import org.lasersonlab.zarr.io.{ Load, Save }
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.zarr.utils.opt.OptCodec

import scala.util.{ Failure, Success, Try }

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
     with Save.syntax
     with Scannable.syntax
     with Zip.syntax
     with xscala.shims
     with VectorEvidence.flat
     with _root_.cats.   syntax.AllSyntax
     with _root_.cats.instances.AllInstances
{

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
