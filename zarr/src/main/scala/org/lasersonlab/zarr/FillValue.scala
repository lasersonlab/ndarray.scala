package org.lasersonlab.zarr

import java.nio.ByteBuffer
import java.util.Base64

import circe._
import lasersonlab.xscala._
import org.lasersonlab.zarr.FillValue.Decoder.Result
import org.lasersonlab.zarr.dtype.DataType

import scala.util.Try

/**
 * TODO: move this into [[DataType]]
 */
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
   * A partially-unapplied [[circe.Decoder]] for [[FillValue]]s: given a [[DataType]] (parsed from [[Metadata]]), parse
   * a [[T]] from some [[Json]]
   *
   * The [[DataType]] is necessary in the case of [[string]] fields, or structs that contain them, because they are
   * encoded as taking up a specific number of bytes, which is discarded in the parsed type [[String]].
   */
  sealed trait Decoder[T]  {
    def apply(json: Json, datatype: DataType[T]): Result[T]
  }
  trait StructDecoder {
    implicit def base64StringDecoder[T]: Decoder[T] =
      new Decoder[T] {
        def apply(
          json: Json,
          datatype: DataType[T]
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
  trait FromDataType
    extends StructDecoder {
    implicit def fromDataType[T](implicit d: DataType[T]): Decoder[T] =
      d match {
        case p: Primitive[T] ⇒
          p match {
            case d @ DataType.   I8     ⇒ Decoder.byte
            case d @ DataType.   I16(_) ⇒ Decoder.short
            case d @ DataType.   I32(_) ⇒ Decoder.int
            case d @ DataType.   I64(_) ⇒ Decoder.long
            case d @ DataType.   F32(_) ⇒ Decoder.float
            case d @ DataType.   F64(_) ⇒ Decoder.double
            case d @ DataType.string(_) ⇒ Decoder.string
          }
        case _ ⇒ base64StringDecoder
      }
  }
  object Decoder
    extends FromDataType {

    type Result[T] = circe.Decoder.Result[FillValue[T]]

    private def make[T](implicit d: circe.Decoder[T]): Decoder[T] =
      new Decoder[T] {
        /**
         * Decoder for types where the [[DataType]] parameter is not necessary (because the JSON representation is
         * always the same size), e.g. numeric types.
         */
        def apply(json: Json, unused: DataType[T]): Result[T] =
          if (json.isNull)
            Right(Null)
          else
            d
              .decodeJson(json)
              .map(FillValue(_))
      }

    implicit val   byte: Decoder[  Byte] = make[  Byte]
    implicit val  short: Decoder[ Short] = make[ Short]
    implicit val    int: Decoder[   Int] = make[   Int]
    implicit val   long: Decoder[  Long] = make[  Long]
    implicit val  float: Decoder[ Float] = make[ Float]
    implicit val double: Decoder[Double] = make[Double]
    implicit val string: Decoder[String] =
      new Decoder[String] {
        val stringDecoder = base64StringDecoder[String]
        def apply(json: Json, datatype: DataType[String]): Result[String] =
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
                  stringDecoder(
                    json,
                    datatype
                  )
              }
      }
  }

  /**
   * In general, decoding a [[FillValue]] requires a [[DataType]], even when the type [[T]] of the [[FillValue]] is
   * known, because e.g. [[string]] fields are encoded as a specific length that is not captured in the decoded
   * [[String]] type (which also affects parsing of [[struct typed]] and [[struct.? untyped]] structs).
   *
   * [[Metadata]]-parsing machinery needs to open the JSON, parse the [[DataType]], and then make that implicitly
   * available in order to get [[FillValue]]-decoding
   */
  implicit def decoder[T](
    implicit
    d: Decoder[T],
    datatype: DataType[T]
  ):
    circe.Decoder[
      FillValue[T]
    ] =
    new circe.Decoder[FillValue[T]] {
      def apply(c: HCursor): Result[T] =
        d(
          c.value,
          datatype
        )
    }

  sealed case class Encoder[T](apply: (T, DataType[T]) ⇒ Json)
  trait LowPriorityEncoder {
    def default[T]: Encoder[T] =
      Encoder {
        (t, datatype) ⇒
          Json.fromString(
            Base64
              .getEncoder
              .encodeToString(datatype(t))
          )
      }
  }
  trait MedPriorityEncoder
    extends LowPriorityEncoder {
    implicit def fromDataType[T](implicit d: DataType[T]): Encoder[T] =
      d match {
        case p: Primitive[T] ⇒
          p match {
            case d @ DataType.   I8     ⇒ Encoder._encodeByte
            case d @ DataType.   I16(_) ⇒ Encoder._encodeShort
            case d @ DataType.   I32(_) ⇒ Encoder._encodeInt
            case d @ DataType.   I64(_) ⇒ Encoder._encodeLong
            case d @ DataType.   F32(_) ⇒ Encoder._encodeFloat
            case d @ DataType.   F64(_) ⇒ Encoder._encodeDouble
            case d @ DataType.string(_) ⇒ Encoder._encodeString
          }
        case _ ⇒
          default[T]
      }
  }
  object Encoder
    extends MedPriorityEncoder {
    import circe.Encoder._
    implicit val _encodeByte  : Encoder[  Byte] = Encoder { (t, _) ⇒ encodeByte  (t) }
    implicit val _encodeShort : Encoder[ Short] = Encoder { (t, _) ⇒ encodeShort (t) }
    implicit val _encodeInt   : Encoder[   Int] = Encoder { (t, _) ⇒ encodeInt   (t) }
    implicit val _encodeLong  : Encoder[  Long] = Encoder { (t, _) ⇒ encodeLong  (t) }
    implicit val _encodeFloat : Encoder[ Float] = Encoder { (t, _) ⇒ encodeFloat (t) }
    implicit val _encodeDouble: Encoder[Double] = Encoder { (t, _) ⇒ encodeDouble(t) }
    implicit val _encodeString: Encoder[String] =
      Encoder {
        case ("", _) ⇒ Json.fromString("")
        case (t, datatype) ⇒
          Json.fromString(
            Base64
              .getEncoder
              .encodeToString(datatype(t))
          )
      }
  }

  implicit def encoder[T](
    implicit
     encoder:  Encoder    [T],
    datatype: DataType[T]
  ):
    circe.Encoder[
      FillValue[T]
    ]
  =
    new circe.Encoder[FillValue[T]] {
      def apply(t: FillValue[T]): Json =
        t match {
          case Null ⇒ Json.Null
          case NonNull(t) ⇒
            encoder.apply(
              t,
              datatype
            )
        }
    }
}
