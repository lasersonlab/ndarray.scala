package org.lasersonlab.zarr

import java.nio.ByteBuffer
import java.util.Base64

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor, Json }
import org.lasersonlab.zarr.dtype.DataType

import scala.util.Try

sealed trait FillValue[+T]

object FillValue {

  import org.relaxng.datatype.Datatype

  implicit def apply[T](t: T): FillValue[T] = NonNull(t)

  case object    Null               extends FillValue[Nothing]
  case  class NonNull[+T](value: T) extends FillValue[T] {
    override def toString: String = value.toString
  }

  object NonNull {
    implicit def unwrap[T](f: NonNull[T]): T = f.value
  }

  import DataType._

  /**
   * A partially-unapplied [[Decoder]] for [[FillValue]]s: given a [[DataType]] (parsed from [[Metadata]]), parse a
   * [[T]] from some [[Json]]
   *
   * The [[DataType]] is necessary in the case of [[string]] fields, or structs that contain them, because they are
   * encoded as taking up a specific number of bytes, which is discarded in the parsed type [[String]].
   */
  sealed trait FillValueDecoder[T]  {
    def apply(json: Json, datatype: DataType.Aux[T]): Result[T]
  }
  trait StructDecoder {
    implicit def decoder[T]: FillValueDecoder[T] =
      new FillValueDecoder[T] {
        def apply(
          json: Json,
          datatype: DataType.Aux[T]
         ):
          Result[T] =
          json
            .as[String]
            .flatMap {
              str ⇒
                Try {
                  datatype(
                    ByteBuffer.wrap(
                      Base64
                        .getDecoder
                        .decode(str)
                    )
                  )
                }
                .fold[Result[T]](
                  err ⇒
                    Left(
                      DecodingFailure.fromThrowable(
                        err,
                        Nil
                      )
                    ),
                  Right(_)
                )
            }
      }
  }
  object FillValueDecoder
    extends StructDecoder {

    private def make[T](implicit d: Decoder[T]): FillValueDecoder[T] =
      new FillValueDecoder[T] {
        /**
         * Decoder for types where the [[DataType]] parameter is not necessary (because the JSON representation is
         * always the same size), e.g. numeric types.
         */
        def apply(json: Json, unused: DataType.Aux[T]): Result[T] =
          d.decodeJson(json)
      }

    implicit val   byte: FillValueDecoder[  Byte] = make[  Byte]
    implicit val  short: FillValueDecoder[ Short] = make[ Short]
    implicit val    i32: FillValueDecoder[   Int] = make[   Int]
    implicit val    i64: FillValueDecoder[  Long] = make[  Long]
    implicit val  float: FillValueDecoder[ Float] = make[ Float]
    implicit val double: FillValueDecoder[Double] = make[Double]
  }

  /**
   * In general, decoding a [[FillValue]] requires a [[DataType]], even when the type [[T]] of the [[FillValue]] is
   * known, because e.g. [[string]] fields are encoded as a specific length that is not captured in the decoded
   * [[String]] type (which also affects parsing of [[Struct typed]] and [[struct untyped]] structs).
   *
   * We implement
 *
   * @param d
   * @param datatype
   * @tparam T
   * @return
   */
  implicit def decoder[T](
    implicit
    d: FillValueDecoder[T],
    datatype: DataType.Aux[T]
  ):
    Decoder[
      FillValue[T]
    ] =
    new Decoder[FillValue[T]] {
      def apply(c: HCursor): Result[FillValue[T]] =
        d(
          c.value,
          datatype
        )
        .map(
          FillValue(_)
        )
    }

  /**
   * When parsing [[untyped.Metadata untyped metadata]], the `fill_value` may be a literal (e.g. a number), or a
   * base64-encoded string or struct; we store it as opaque [[Json]], and this [[Decoder]] parses it
   */
  implicit val decodeJson: Decoder[FillValue[Json]] =
    new Decoder[FillValue[Json]] {
      def apply(c: HCursor): Result[FillValue[Json]] = Right(c.value)
    }
}
