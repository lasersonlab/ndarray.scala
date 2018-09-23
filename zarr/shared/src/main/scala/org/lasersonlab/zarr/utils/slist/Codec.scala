package org.lasersonlab.zarr.utils.slist

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json, Printer }
import lasersonlab.shapeless.slist._
import org.lasersonlab.shapeless.SList.FromList.{ TooFew, TooMany }
import org.lasersonlab.shapeless.SList.{ FromList, ToList }

trait HKTDecoder[F[_]] {
  def apply[T](implicit d: Decoder[T]): Decoder[F[T]]
}

trait HKTEncoder[F[_]] {
  def apply[T](implicit d: Encoder[T]): Encoder[F[T]]
}

trait Codecs {
  type HKTCodec[F[_]] = HKTEncoder[F] with HKTDecoder[F]
  type Codec[T] = Encoder[T] with Decoder[T]

  implicit val codec_0: HKTCodec[`0`] =
    new HKTEncoder[`0`] with HKTDecoder[`0`] {

      override def apply[T](implicit d: Encoder[T]): Encoder[`0`[T]] =
        new Encoder[`0`[T]] {
          override def apply(a: `0`[T]): Json = Json.arr()
        }

      override def apply[T](implicit d: Decoder[T]): Decoder[`0`[T]] =
        new Decoder[`0`[T]] {
          override def apply(c: HCursor): Result[`0`[T]] =
            c
              .as[List[T]]
              .flatMap {
                case Nil ⇒
                  Right(⊥)
                case extra ⇒
                  val print = Printer.spaces4.copy(colonLeft = "").pretty _
                  Left(
                    DecodingFailure(
                      s"Found ${extra.size} extra elements: ${print(c.value)}",
                      c.history
                    )
                  )
              }
        }
    }

  implicit def make[F[_]](implicit to: ToList[F], from: FromList[F]): HKTCodec[F] =
    new HKTEncoder[F] with HKTDecoder[F] {
      override def apply[T](implicit d: Encoder[T]): Encoder[F[T]] =
        new Encoder[F[T]] {
          override def apply(a: F[T]): Json =
            Json.arr(
              to(a)
              .map(d(_))
                : _*
            )
        }

      override def apply[T](implicit d: Decoder[T]): Decoder[F[T]] =
        new Decoder[F[T]] {
          override def apply(c: HCursor): Result[F[T]] =
            c
              .as[List[T]]
              .flatMap(
                from(_)
                  .left
                  .map {
                    err ⇒
                      val print = Printer.spaces4.copy(colonLeft = "").pretty _
                      DecodingFailure(
                        err match {
                          case TooFew (    by) ⇒ s"Expected ${from.size} elements; found ${from.size -          by}: ${print(c.value)}"
                          case TooMany(extras) ⇒ s"Expected ${from.size} elements, found ${from.size + extras.size}: ${print(c.value)}"
                        },
                        c.history
                      )
                  }
              )
        }
    }

  implicit def makeEncoder[F[_], T](implicit hkt: HKTEncoder[F], d: Encoder[T]): Encoder[F[T]] = hkt(d)
  implicit def makeDecoder[F[_], T](implicit hkt: HKTDecoder[F], d: Decoder[T]): Decoder[F[T]] = hkt(d)

//  implicit val codec_1 = make[`0`]
//  implicit val codec_2 = make[`1`]
//  implicit val codec_3 = make[`2`]
//  implicit val codec_4 = make[`3`]
//  implicit val codec_5 = make[`4`]
//  implicit val codec_6 = make[`5`]
}
object Codecs extends Codecs
