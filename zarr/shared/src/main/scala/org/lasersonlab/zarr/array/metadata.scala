package org.lasersonlab.zarr.array

import cats.implicits._
import cats.{ Functor, Traverse }
import hammerlab.option.Opt
import hammerlab.path.Path
import io.circe.generic.auto._
import org.lasersonlab.circe.{ DecoderK, EncoderK }
import org.lasersonlab.shapeless.Zip
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.FillValue.Null
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr._
import org.lasersonlab.zarr.circe.Decoder.Result
import org.lasersonlab.zarr.circe._
import org.lasersonlab.zarr.circe.parser._
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.Basename
import org.lasersonlab.zarr.utils.Idx

object metadata {
  sealed trait Interface {
    val shape: Shape[Dimension[Idx]]
    val dtype: DataType
    val compressor: Compressor = Blosc()
    val order: Order = C
    val fill_value: FillValue[T] = Null
    val zarr_format: Format = `2`
    val filters: Opt[Seq[Filter]] = None

    type T = dtype.T
    type Shape[_]
    type Idx

    /** Allows a caller to coerce the type of a [[Interface]] to include its constituent types */
    def t = this.asInstanceOf[zarr.Metadata[Shape, Idx, T]]  // TODO use match / sealedness instead
  }

  type Shaped[_Shape[_], _I] =
    Interface {
      type Shape[U] = _Shape[U]
      type Idx = _I
    }

  object untyped {
    import Metadata.basename
    def apply(dir: Path)(implicit idx: Idx): Exception | Shaped[List, idx.T] =
      dir ? basename flatMap {
        path ⇒
          decode[
            Shaped[
              List,
              idx.T
            ]
          ](
            path.read
          )
      }

    implicit def decoder(
      implicit
      idx: Idx
    ):
      Decoder[
        Shaped[
          List,
          idx.T
        ]
      ] =
      new Decoder[Shaped[List, idx.T]] {
        type Idx = idx.T
        def apply(c: HCursor): Result[Shaped[List, Idx]] = {
          for {
                  _shape ← c.downField(      "shape").as[List[Idx]]
                 _chunks ← c.downField(     "chunks").as[List[Chunk.Idx]]
                  _dtype ← c.downField(      "dtype").as[DataType]
             _compressor ← c.downField( "compressor").as[Compressor]
                  _order ← c.downField(      "order").as[Order]
            _zarr_format ← c.downField("zarr_format").as[Format]
          } yield
            if (_shape.size == _chunks.size)
              new Metadata[List, Idx, _dtype.T](
                _shape
                  .zip(_chunks)
                  .map {
                    case (arr, chunk) ⇒
                      Dimension(arr, chunk)
                  },
                _dtype,
                _compressor,
                _order,
                // TODO: move fill-value parsing into datatype
                FillValue.Null,
                _zarr_format,
                c
                  .downField("filters")
                  .as[Seq[Filter]]
                  .toOption
              )
            else
              throw new IllegalStateException(
                s"Shape and chunks arrays have unequal sizes (${_shape.size} vs ${_chunks.size}): ${_shape.mkString(",")} ${_chunks.mkString(",")}"
              )
        }
      }
  }

  case class Metadata[_Shape[_], _Idx, _T](
     shape: _Shape[Dimension[_Idx]],
    dtype: DataType.Aux[_T],
     override val compressor: Compressor = Blosc(),
     override val order: Order = C,
     override val fill_value: FillValue[_T] = Null,
     override val zarr_format: Format = `2`,
     override val filters: Opt[Seq[Filter]] = None
  )
  extends Interface
  {
    type Shape[U] = _Shape[U]
    type Idx = _Idx
  }

  object Metadata {
    val basename = ".zarray"
    implicit def _basename[Shape[_], Idx, T]: Basename[Metadata[Shape, Idx, T]] = Basename(basename)

    // Implicit unwrappers for some fields
    implicit def _compressor[S[_]   ](implicit md: Metadata[S, _, _]):      Compressor = md.compressor
    implicit def _datatype  [S[_], T](implicit md: Metadata[S, _, T]): DataType.Aux[T] = md.     dtype

    def apply[
          T   : DataType.Decoder : FillValue.Decoder,
      Shape[_]: Functor : Zip : DecoderK,
        Idx   : Decoder
    ](
      dir: Path
    ):
      Exception |
      Metadata[Shape, Idx, T]
    =
      dir ? basename flatMap {
        path ⇒
          decode[Metadata[Shape, Idx, T]](
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
          T   : FillValue.Decoder,
      Shape[_]: Functor : Zip : DecoderK,
        Idx   : Decoder
    ](
      implicit
      datatypeDecoder: DataType.Decoder[T]
    ):
      Decoder[
        Metadata[
          Shape,
          Idx,
          T
        ]
      ] =
      new Decoder[Metadata[Shape, Idx, T]] {
        def apply(c: HCursor):
          Result[
            Metadata[
              Shape,
              Idx,
              T
            ]
          ]
        =
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
                    zarr_format,
                    filters =
                      c
                        .downField("filters")
                        .as[Seq[Filter]]
                        .toOption
                  )
            }
      }

    implicit def encoder[
          T   : DataType.Encoder : FillValue.Encoder,
      Shape[_]: Traverse : EncoderK,
        Idx   : Encoder
    ]:
      Encoder[
        Metadata[
          Shape,
          Idx,
          T
        ]
      ]
    =
      new Encoder[Metadata[Shape, Idx, T]] {
        def apply(m: Metadata[Shape, Idx, T]): Json = {
          implicit val datatype = m.dtype
          implicit val enc = FillValue.encoder[T]
          Json.obj(
                  "shape" → encode(m.shape.map(_.size)),
                 "chunks" → encode(m.shape.map(_.chunk)),
             "compressor" → encode(m.compressor),
                  "dtype" → encode(m.dtype),
                  "order" → encode(m.order),
             "fill_value" → encode(m.fill_value),
            "zarr_format" → encode(m.zarr_format),
                "filters" → encode(m.filters)
          )
        }
      }
  }
}
