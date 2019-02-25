package org.lasersonlab.zarr.array

import cats.Traverse
import org.lasersonlab.circe.DecoderK
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

import scala.concurrent.ExecutionContext

object metadata {

  val basename = ".zarray"

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

  object ? {
    implicit def _basename[Shape[_], Idx]: Basename[?[Shape, Idx]] = Basename(basename)

    /**
     * Load an [[? "untyped"]] [[Metadata]] from the path to a containing directory
     *
     * @param dir directory containing `.zarray` metadata file
     * @param idx "index" type to use for dimensions' coordinates
     */
    def apply(dir: Path)(implicit idx: Idx, ec: ExecutionContext): F[?[List, idx.T]] =
      (dir ? basename).flatMap {
        _
          .string
          .map {
            str ⇒
            decode[metadata.?[List, idx.T]](str): Throwable | metadata.?[List, idx.T]
          }
      }
      .rethrow

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

    implicit def
        encoder[  Shape[_], Idx ]:
        Encoder[?[Shape   , Idx]] =
    new Encoder[?[Shape   , Idx]] {
      @inline def apply(m: ?[Shape, Idx]): Json = encode(m.t)
    }
  }

  case class Metadata[
    Shape[_]: Traverse,
      Idx   : Idx.T,
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

    def json: Json = {
      val (_shape, _chunks) =
        shape
          .toList
          .map {
            s ⇒
              (s.size, s.chunk)
          }
          .unzip

      implicit val datatype = dtype
      implicit val encodeIdx = implicitly[Idx.T[Idx]].encoder

      Json.obj(
              "shape" → encode(_shape),
             "chunks" → encode(_chunks),
         "compressor" → encode(compressor),
              "dtype" → encode(dtype),
              "order" → encode(order),
         "fill_value" → encode(fill_value),
        "zarr_format" → encode(zarr_format),
            "filters" → encode(filters)
      )
    }

    override def toString: String = pprint(json)
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
      idx: Idx,
      ec: ExecutionContext
    ):
      F[Metadata[Shape, idx.T, T]]
    =
      (dir ? basename).flatMap {
        _
          .string
          .map {
            decode[
              Metadata[
                Shape,
                idx.T,
                T
              ]
            ](_): Throwable | Metadata[Shape, idx.T, T]
          }
      }
      .rethrow

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

    implicit def
        encoder[            Shape[_], Idx, T]:
        Encoder[   Metadata[Shape   , Idx, T]] =
    new Encoder[   Metadata[Shape   , Idx, T]] {
      def apply(m: Metadata[Shape   , Idx, T]): Json = m.json
    }
  }
}
