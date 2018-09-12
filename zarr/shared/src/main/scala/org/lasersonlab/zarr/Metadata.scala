package org.lasersonlab.zarr

import _root_.io.circe.Decoder.Result
import _root_.io.circe._
import _root_.io.circe.parser._
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.FillValue.Null
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.Basename

case class Metadata[_T, _Shape](
   shape: _Shape,
  chunks: _Shape,
  dtype: DataType.Aux[_T],
   override val compressor: Compressor = Blosc(),
   override val order: Order = C,
   override val fill_value: FillValue[_T] = Null,
   override val zarr_format: Format = `2`,
   override val filters: Opt[Seq[Filter]] = None
)
extends untyped.Metadata
{
  type Shape = _Shape
}

object Metadata {

  val basename = ".zarray"
  implicit def _basename[T, Shape] = Basename[Metadata[T, Shape]](basename)

  def apply[
        T : FillValue.Decoder,
    Shape :           Decoder
  ](
    dir: Path
  )(
    implicit
    d: Decoder[DataType.Aux[T]]
  ):
    Exception |
    Metadata[T, Shape]
  =
    dir ? basename flatMap {
      path ⇒
        decode[Metadata[T, Shape]](
          path.read
        )
    }

  /**
   * To JSON-decode a [[Metadata]], we can mostly use [[_root_.io.circe.generic.auto]], however there is one wrinkle:
   * [[Metadata.fill_value `fill_value`]]s for structs are stored in JSON as base64-encoded strings.
   *
   * Complicating matters further, decoding such a blob can require knowing lengths of multiple
   * [[org.lasersonlab.zarr.dtype.DType.string]]-typed fields, which we don't store in the type-system.
   *
   * So, we decode [[Metadata]] by:
   *
   * - first, decoding the [[Metadata.dtype `dtype`]] field to get the complete picture of the [[DataType]]
   * - creating a [[Metadata.fill_value `fill_value`]]-[[Decoder]] with this information
   * - running a normal generic auto-derivation to get a [[Metadata]]-[[Decoder]]
   */
  implicit def decoder[
        T: FillValue.Decoder,
    Shape:           Decoder
  ](
    implicit
    datatypeDecoder: Decoder[DataType.Aux[T]]
  ):
    Decoder[
      Metadata[
        T,
        Shape
      ]
    ] =
    new Decoder[Metadata[T, Shape]] {
      def apply(c: HCursor):
        Result[
          Metadata[
            T,
            Shape
          ]
        ]
      = {
        c
          .downField("dtype")
          .success
          .fold[DecodingFailure | HCursor](
            Left(
              DecodingFailure(
                "",
                c.history
              )
            )
          )(
            Right(_)
          )
          .flatMap {
            datatypeDecoder(_)
          }
          .flatMap {
            implicit datatype ⇒
              import _root_.io.circe.generic.auto._
              exportDecoder[Metadata[T, Shape]].instance(c)
          }
      }
    }

  implicit def encoder[
        T: FillValue.Encoder,
    Shape:           Encoder
  ](
    implicit
    datatypeEncoder: Encoder[DataType.Aux[T]]
  ):
    Encoder[
      Metadata[
        T,
        Shape
      ]
    ]
  =
    new Encoder[Metadata[T, Shape]] {
      def apply(m: Metadata[T, Shape]): Json = {
        implicit val datatype = m.dtype
        implicit val enc = FillValue.encoder[T]
        import _root_.io.circe.generic.auto._
        exportEncoder[Metadata[T, Shape]].instance(m)
      }
    }
}
