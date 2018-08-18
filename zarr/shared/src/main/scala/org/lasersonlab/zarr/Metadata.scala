package org.lasersonlab.zarr

import java.io.FileNotFoundException
import java.nio.ByteBuffer
import java.util.Base64

import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.{ Decoder, DecodingFailure, HCursor, Json }
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr.dtype.DataType

case class Metadata[T, Shape](
  shape: Shape,
  chunks: Shape,
  dtype: DataType.Aux[T],
  compressor: Compressor,
  order: Order,
  fill_value: Opt[T] = None,
  zarr_format: Format = `2`,
  filters: Opt[Seq[Filter]] = None
)

object Metadata {
  val basename = ".zarray"

  // Implicit unwrappers for some fields
  implicit def _compressor[T, Shape](implicit md: Metadata[_, _]): Compressor = md.compressor
  implicit def   _datatype[T, Shape](implicit md: Metadata[T, _]): DataType.Aux[T] = md.dtype

  def apply[
        T : Decoder,
    Shape : Decoder
  ](
    dir: Path
  )(
    implicit
    d: Decoder[DataType.Aux[T]]
  ):
    Exception |
    Metadata[T, Shape]
  = {
    val path = dir / basename
    if (!path.exists)
      Left(
        new FileNotFoundException(
          path.toString
        )
      )
    else
      decode[
        Metadata[
          T,
          Shape
        ]
      ](
        path.read
      )(
        this.decoder[T, Shape]
      )
  }

  def decoder[
    T,
    Shape: Decoder
  ](
    implicit
    datatypeDecoder: Decoder[DataType.Aux[T]],
    elementDecoder: Decoder[T]
  ): Decoder[Metadata[T, Shape]] =
    new Decoder[Metadata[T, Shape]] {
      val elemDecoder = elementDecoder
      def apply(c: HCursor): Result[Metadata[T, Shape]] = {
        c
          .downField("dtype")
          .success
          .map(Right(_))
          .getOrElse(
            Left(
              DecodingFailure(
                "",
                c.history
              )
            )
          )
          .flatMap {
            datatypeDecoder(_)
          }
          .flatMap {
            datatype ⇒
              implicit val elementDecoder: Decoder[Opt[T]] =
                new Decoder[Opt[T]] {
                  def apply(c: HCursor): Result[Opt[T]] =
                    c.value match {
                      case j if j.isNull ⇒ Right(None)
                      case j ⇒
                        j
                          .asString
                          .fold(
                            elemDecoder
                              .decodeJson(j)
                          ) {
                            base64 ⇒
                              val decodedBytes =
                                Base64
                                .getDecoder
                                .decode(base64)

                              Right(
                                datatype(
                                  ByteBuffer.wrap(
                                    decodedBytes
                                  )
                                )
                              )
                          }
                          .map { x ⇒ x }  // cast to Opt
                    }
                }

              val decoder = implicitly[Decoder[Metadata[T, Shape]]]
              decoder(c)
          }
      }
    }

  object untyped {
    case class Metadata(
       shape: Seq[Int],
      chunks: Seq[Int],
      dtype: DataType,
      compressor: Compressor,
      order: Order,
      fill_value: Opt[Json] = None,
      zarr_format: Format = `2`,
      filters: Opt[Seq[Filter]] = None
    )
  }
}
