package org.lasersonlab.zarr

import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder.Result
import io.circe.parser._
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.lasersonlab.zarr.FillValue.{ FillValueDecoder, Null }
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr.dtype.DataType

case class Metadata[T, Shape](
  shape: Shape,
  chunks: Shape,
  dtype: DataType.Aux[T],
  compressor: Compressor,
  order: Order,
  fill_value: FillValue[T] = Null,
  zarr_format: Format = `2`,
  filters: Opt[Seq[Filter]] = None
)

object Metadata {
  val basename = ".zarray"

  // Implicit unwrappers for some fields
  implicit def _compressor[T, Shape](implicit md: Metadata[_, _]): Compressor = md.compressor
  implicit def   _datatype[T, Shape](implicit md: Metadata[T, _]): DataType.Aux[T] = md.dtype

  def apply[
        T : FillValueDecoder,
    Shape :          Decoder
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
   * To JSON-decode a [[Metadata]], we can mostly use [[io.circe.generic.auto]], however there is one wrinkle:
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
        T: FillValueDecoder,
    Shape:          Decoder
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
              import io.circe.generic.auto._
              exportDecoder[Metadata[T, Shape]].instance(c)
          }
      }
    }
}
