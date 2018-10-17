package org.lasersonlab.zarr.array

import cats.Traverse
import hammerlab.path.Path
import org.lasersonlab.circe.{ DecoderK, EncoderK }
import org.lasersonlab.slist.Zip
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
import shapeless.the

object metadata {

  /**
   * Base-trait for [[Metadata]] where type-params are type-members, allowing for construction in situations where
   * relevant types (particularly [[Metadata.T]], the element-type) are not known ahead of time.
   *
   * The only implementation of this interface is [[Metadata]], and generally the interface should not be used
   * directly outside this file
   */
  sealed abstract class ?[Shape[_], Idx] {
    val       shape: Shape[Dimension[Idx]]
    val       dtype:           DataType[T]
    val  compressor:         Compressor    = Blosc()
    val       order:              Order    =     C
    val  fill_value:          FillValue[T] =  Null
    val zarr_format:             Format    =    `2`
    val     filters:  Option[Seq[Filter]]  =  None

    type T

    /** Narrow a [[?]] to its underlying [[Metadata]] */
    def t =
      this match {
        case m: Metadata[Shape, Idx, T] ⇒ m
      }
  }

  val basename = ".zarray"

  object ? {
    implicit def _basename[Shape[_], Idx]: Basename[?[Shape, Idx]] = Basename(basename)

    /**
     * Load an [[? "untyped"]] [[Metadata]] from the path to a containing directory
     *
     * @param dir directory containing `.zarray` metadata file
     * @param idx "index" type to use for dimensions' coordinates
     */
    def apply(dir: Path)(implicit idx: Idx): Exception | ?[List, idx.T] =
      dir ? basename flatMap {
        path ⇒
          decode[
            ?[
              List,
              idx.T
            ]
          ](
            path.read
          )
      }

    implicit def decoder[
      Shape[_]
      : DecoderK
      : Zip
      : Traverse
    ](
      implicit
      idx: Idx
    ):
      Decoder[
        ?[
          Shape,
          idx.T
        ]
      ] =
      new Decoder[
        ?[
          Shape,
          idx.T
        ]
      ] {
        type Idx = idx.T
        import Dimensions.decodeList
        def apply(c: HCursor): Result[?[Shape, Idx]] =
          for {
              dimensions ←   c.as[Shape[Dimension[idx.T]]]
                  _dtype ←   c.downField(      "dtype").as[DataType.?]
             _compressor ←   c.downField( "compressor").as[Compressor]
                  _order ←   c.downField(      "order").as[Order]
            _zarr_format ←   c.downField("zarr_format").as[Format]
             _fill_value ← {
                             implicit val d: DataType[_dtype.T] = _dtype.t
                             c.downField("fill_value").as[FillValue[_dtype.T]]
                           }
          } yield
            Metadata[Shape, Idx, _dtype.T](
              dimensions,
              _dtype.t,
              _compressor,
              _order,
              _fill_value,
              _zarr_format,
              c
                .downField("filters")
                .as[Seq[Filter]]
                .toOption
            )
            : ?[Shape, Idx]
      }

    implicit def encoder[
      Shape[_]
             : Traverse
             : EncoderK,
        Idx  : Encoder
    ]:
      Encoder[
        ?[
          Shape,
          Idx
        ]
      ]
    =
      new Encoder[
        ?[
          Shape,
          Idx
        ]
      ] {
        @inline def apply(m: ?[Shape, Idx]): Json =
          Metadata.encoder[Shape, Idx, m.T].apply(m.t)
      }
  }

  case class Metadata[
    Shape[_],
      Idx,
       _T
  ](
                       shape: Shape[Dimension[Idx]],
                       dtype:          DataType[_T],
    override val  compressor:        Compressor    = Blosc(),
    override val       order:             Order    = C,
    override val  fill_value:         FillValue[_T] = Null,
    override val zarr_format:            Format    = `2`,
    override val     filters: Option[Seq[Filter]]  = None
  )
  extends ?[Shape, Idx] {
    type T = _T
  }

  object Metadata {
    // Implicit unwrappers for some fields
    implicit def _compressor[S[_]   ](implicit md: Metadata[S, _, _]):      Compressor = md.compressor
    implicit def _datatype  [S[_], T](implicit md: Metadata[S, _, T]): DataType[T] = md.     dtype

    def apply[
      Shape[_]
             : Traverse
             : Zip
             : DecoderK,
          T
             :  DataType.Decoder
             : FillValue.Decoder,
    ](
      dir: Path
    )(
      implicit
      idx: Idx
    ):
      Exception |
      Metadata[Shape, idx.T, T]
    =
      dir ? basename flatMap {
        path ⇒
          decode[
            Metadata[
              Shape,
              idx.T,
              T
            ]
          ](
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
      Shape[_]: Traverse : Zip : DecoderK,
        Idx,
          T   : FillValue.Decoder,
    ](
      implicit
      datatypeDecoder: DataType.Decoder[T],
      idx: Idx.T[Idx]
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
          // first decode the datatype, then use it to decode the fill-value (and everything else); fill-value-decoding
          // may need to know the expected length of fixed-length string fields, which can vary despite the downstream
          // type exposed being simply "String"
          c
            .downField("dtype")
            .success
            .fold[
              DecodingFailure |
              HCursor
            ](
              Left(
                DecodingFailure(
                  s"No 'dtype' field on metadata: ${pprint(c.value)}",
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
                   dimensions ← c.as[Shape[Dimension[Idx]]](Dimensions.decodeList[Shape])
                   compressor ← c.downField( "compressor").as[Compressor]
                        order ← c.downField(      "order").as[Order]
                  fill_value  ← c.downField( "fill_value").as[FillValue[T]]
                  zarr_format ← c.downField("zarr_format").as[Format]
                } yield
                  Metadata(
                    dimensions,
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
      Shape[_]: Traverse : EncoderK,
        Idx   : Encoder,
          T
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
          val edt = the[FillValue.Encoder[T]]
          implicit val enc = FillValue.encoder[T](FillValue.Encoder.fromDataType, datatype)
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
