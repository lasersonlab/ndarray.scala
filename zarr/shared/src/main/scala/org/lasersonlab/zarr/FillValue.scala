package org.lasersonlab.zarr

import java.nio.ByteBuffer
import java.util.Base64

import _root_.io.circe._
import org.lasersonlab.zarr.FillValue.FillValueDecoder.Result
import org.lasersonlab.zarr.dtype.DataType

import scala.util.Try

sealed trait FillValue[+T]

object FillValue {

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
   *
   * TODO: make this FillValue.Decoder, likewise for Encoder
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
          if (json.isNull)
            Right(Null)
          else
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

    type Result[T] = Decoder.Result[FillValue[T]]

    private def make[T](implicit d: Decoder[T]): FillValueDecoder[T] =
      new FillValueDecoder[T] {
        /**
         * Decoder for types where the [[DataType]] parameter is not necessary (because the JSON representation is
         * always the same size), e.g. numeric types.
         */
        def apply(json: Json, unused: DataType.Aux[T]): Result[T] =
          if (json.isNull)
            Right(Null)
          else
            d
              .decodeJson(json)
              .map(FillValue(_))
      }

    implicit val   byte: FillValueDecoder[  Byte] = make[  Byte]
    implicit val  short: FillValueDecoder[ Short] = make[ Short]
    implicit val    int: FillValueDecoder[   Int] = make[   Int]
    implicit val   long: FillValueDecoder[  Long] = make[  Long]
    implicit val  float: FillValueDecoder[ Float] = make[ Float]
    implicit val double: FillValueDecoder[Double] = make[Double]
    implicit val string: FillValueDecoder[String] =
      new FillValueDecoder[String] {
        val base64Decoder = decoder[String]
        def apply(json: Json, datatype: DataType.Aux[String]): Result[String] =
          if (json.isNull)
            Right(Null)
          else
            json
              .as[String]
              .flatMap {
                // TODO: this is probably a bug in the spec (or reference implementation): zarr datasets in the wild use
                // "" as `fill_value` for "string" datatypes (e.g. "|S12"), but the spec says:
                //
                // """
                // If an array has a fixed length byte string data type (e.g., "|S12"), or a structured data type, and
                // if the fill value is not null, then the fill value MUST be encoded as an ASCII string using the
                // standard Base64 alphabet.
                // """
                //
                // In my reading, the `fill_value` for "|S12" should be either `null` or e.g. "AAAAAAAAAAAAAAAA=" to
                // represent an empty string; "" is not a valid base64-encoded 12-byte string… or, if it is, what about
                // other base64-encoded strings of length less than 16 (corresponding to 12 decoded bytes)? Are we only
                // providing the least-significant bytes / a suffix of the actual value, and assuming left-padding with
                // 0's ("A"s)?
                case "" ⇒ Right("")
                case _ ⇒
                  base64Decoder(
                    json,
                    datatype
                  )
              }
      }
  }

  /**
   * In general, decoding a [[FillValue]] requires a [[DataType]], even when the type [[T]] of the [[FillValue]] is
   * known, because e.g. [[string]] fields are encoded as a specific length that is not captured in the decoded
   * [[String]] type (which also affects parsing of [[Struct typed]] and [[struct untyped]] structs).
   *
   * [[Metadata]]-parsing machinery needs to open the JSON, parse the [[DataType]], and then make that implicitly
   * available in order to get [[FillValue]]-decoding
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
      def apply(c: HCursor): Result[T] =
        d(
          c.value,
          datatype
        )
    }

  /**
   * When parsing [[untyped.Metadata untyped metadata]], the `fill_value` may be a literal (e.g. a number), or a
   * base64-encoded string or struct; we store it as opaque [[Json]], and this [[Decoder]] parses it
   */
  implicit val decodeJson: Decoder[FillValue[Json]] =
    new Decoder[FillValue[Json]] {
      def apply(c: HCursor): Result[Json] = Right(c.value)
    }

  sealed class FillValueEncoder[T](val apply: (T, DataType.Aux[T]) ⇒ Json)
  trait LowPriorityFillValueEncoder {
    case class Make[T](f: (T, DataType.Aux[T]) ⇒ Json) extends FillValueEncoder(f)
    implicit def default[T]: FillValueEncoder[T] =
      Make {
        (t, datatype) ⇒
          Json.fromString(
            Base64
              .getEncoder
              .encodeToString(datatype(t))
          )
      }
  }
  object FillValueEncoder
    extends LowPriorityFillValueEncoder {
    import Encoder._
    implicit val _encodeByte  : FillValueEncoder[  Byte] = Make { (t, _) ⇒ encodeByte  (t) }
    implicit val _encodeShort : FillValueEncoder[ Short] = Make { (t, _) ⇒ encodeShort (t) }
    implicit val _encodeInt   : FillValueEncoder[   Int] = Make { (t, _) ⇒ encodeInt   (t) }
    implicit val _encodeLong  : FillValueEncoder[  Long] = Make { (t, _) ⇒ encodeLong  (t) }
    implicit val _encodeFloat : FillValueEncoder[ Float] = Make { (t, _) ⇒ encodeFloat (t) }
    implicit val _encodeDouble: FillValueEncoder[Double] = Make { (t, _) ⇒ encodeDouble(t) }
    implicit val _encodeString: FillValueEncoder[String] =
      Make {
        case ("", _) ⇒ Json.fromString("")
        case (t, datatype) ⇒
          Json.fromString(
            Base64
              .getEncoder
              .encodeToString(datatype(t))
          )
      }
  }

  implicit val encodeJson: Encoder[FillValue[Json]] =
    new Encoder[FillValue[Json]] {
      def apply(a: FillValue[Json]): Json =
        a match {
          case Null ⇒ Json.Null
          case NonNull(json) ⇒ json
        }
    }

  implicit def encoder[T](
    implicit
    fillValueEncoder: FillValueEncoder[T],
    datatype: DataType.Aux[T]
  ):
    Encoder[
      FillValue[T]
    ]
  =
    new Encoder[FillValue[T]] {
      def apply(t: FillValue[T]): Json =
        t match {
          case Null ⇒ Json.Null
          case NonNull(t) ⇒
            fillValueEncoder.apply(
              t,
              datatype
            )
        }
    }
}
