package org.lasersonlab.zarr

import java.nio.ByteBuffer
import java.util.Base64

import io.circe.Decoder.Result
import io.circe.{ Decoder, HCursor, Json }
import org.lasersonlab.zarr.dtype.DataType

sealed trait FillValue[+T]

object FillValue {
  implicit def apply[T](t: T): FillValue[T] = NonNull(t)

  case class NonNull[+T](value: T) extends FillValue[T] {
    override def toString: String = value.toString
  }

  object NonNull {
    implicit def unwrap[T](f: NonNull[T]): T = f.value
  }

  case object Null extends FillValue[Nothing]

  sealed trait FillValueDecoder[T]  {
    def apply(json: Json, datatype: DataType.Aux[T]): Result[T]
  }
  trait StructDecoder {
    implicit def decoder[T](implicit d: Decoder[T]): FillValueDecoder[T] =
      new FillValueDecoder[T] {
        def apply(json: Json, datatype: DataType.Aux[T]): Result[T] =
          json
            .as[String]
            .map {
              str ⇒
                datatype(
                  ByteBuffer.wrap(
                    Base64
                      .getDecoder
                      .decode(str)
                  )
                )
            }
      }
  }
  object FillValueDecoder
    extends StructDecoder {
    private def make[T](implicit d: Decoder[T]): FillValueDecoder[T] =
      new FillValueDecoder[T] {
        override def apply(json: Json, datatype: DataType.Aux[T]): Result[T] =
          d.decodeJson(json)
      }
    implicit val   char: FillValueDecoder[  Char] = make[  Char]
    implicit val  short: FillValueDecoder[ Short] = make[ Short]
    implicit val    i32: FillValueDecoder[   Int] = make[   Int]
    implicit val    i64: FillValueDecoder[  Long] = make[  Long]
    implicit val  float: FillValueDecoder[ Float] = make[ Float]
    implicit val double: FillValueDecoder[Double] = make[Double]
    implicit val string: FillValueDecoder[String] = make[String]
  }

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

  implicit val decodeJson: Decoder[FillValue[Json]] =
    new Decoder[FillValue[Json]] {
      override def apply(c: HCursor): Result[FillValue[Json]] = Right(c.value)
    }
}
