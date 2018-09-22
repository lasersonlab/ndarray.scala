package org.lasersonlab.zarr

import cats.{ Functor, Semigroupal, Traverse }
import cats.implicits._
import circe.Decoder.Result
import circe._
import circe.parser._
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.FillValue.Null
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.Basename

case class Metadata[_T, _Shape[_], _Idx](
   shape: _Shape[Dimension[_Idx]],
  dtype: DataType.Aux[_T],
   override val compressor: Compressor = Blosc(),
   override val order: Order = C,
   override val fill_value: FillValue[_T] = Null,
   override val zarr_format: Format = `2`,
   override val filters: Opt[Seq[Filter]] = None
)
extends untyped.Metadata
{
  type Shape[U] = _Shape[U]
  type Idx = _Idx
}

object Metadata {

  val basename = ".zarray"
  implicit def _basename[T, Shape[_], Idx]: Basename[Metadata[T, Shape, Idx]] = Basename(basename)

  // Implicit unwrappers for some fields
  implicit def _compressor[   S[_]](implicit md: Metadata[_, S, _]):      Compressor = md.compressor
  implicit def _datatype  [T, S[_]](implicit md: Metadata[T, S, _]): DataType.Aux[T] = md.     dtype

  def apply[
        T : FillValue.Decoder,
    Shape[_]: Functor : Semigroupal,
      Idx
  ](
    dir: Path
  )(
    implicit
    d: Decoder[DataType.Aux[T]],
    ds: Decoder[Shape[Idx]],
    cds: Decoder[Shape[Chunk.Idx]]
  ):
    Exception |
    Metadata[T, Shape, Idx]
  =
    dir ? basename flatMap {
      path ⇒
        decode[Metadata[T, Shape, Idx]](
          path.read
        )
    }

  /**
   * To JSON-decode a [[Metadata]], we can mostly use [[circe.auto]], however there is one wrinkle:
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
    Shape[_]: Functor : Semigroupal,
      Idx
  ](
    implicit
    datatypeDecoder: Decoder[DataType.Aux[T]],
    // TODO: HKT Shape decoder
    ds: Decoder[Shape[Idx]],
    cds: Decoder[Shape[Chunk.Idx]]
  ):
    Decoder[
      Metadata[
        T,
        Shape,
        Idx
      ]
    ] =
    new Decoder[Metadata[T, Shape, Idx]] {
      def apply(c: HCursor):
        Result[
          Metadata[
            T,
            Shape,
            Idx
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
              for {
                        arr ← c.downField(      "shape").as[Shape[Idx]]
                     chunks ← c.downField(     "chunks").as[Shape[Chunk.Idx]]
                 compressor ← c.downField( "compressor").as[Compressor]
                      order ← c.downField(      "order").as[Order]
                fill_value  ← c.downField( "fill_value").as[FillValue[T]]
                zarr_format ← c.downField("zarr_format").as[Format]
              } yield
                Metadata(
                  Dimensions(arr, chunks),
                  datatype,
                  compressor,
                  order,
                  fill_value,
                  zarr_format
                )
          }
      }
    }

  implicit def encoder[
        T: FillValue.Encoder,
    Shape[_] : Traverse,
      Idx
  ](
    implicit
    datatypeEncoder: Encoder[DataType.Aux[T]],
    ds: Encoder[Shape[Idx]],
    cds: Encoder[Shape[Chunk.Idx]]
  ):
    Encoder[
      Metadata[
        T,
        Shape,
        Idx
      ]
    ]
  =
    new Encoder[Metadata[T, Shape, Idx]] {
      def apply(m: Metadata[T, Shape, Idx]): Json = {
        implicit val datatype = m.dtype
        implicit val enc = FillValue.encoder[T]
        Json.obj(
          "shape" → encode(m.shape.map(_.arr)),
          "chunks" → encode(m.shape.map(_.chunk)),
          "compressor" → encode(m.compressor),
          "dtype" → encode(m.dtype),
          "order" → encode(m.order),
          "fill_value" → encode(m.fill_value),
          "zarr_format" → encode(m.zarr_format)
        )
      }
    }
}
