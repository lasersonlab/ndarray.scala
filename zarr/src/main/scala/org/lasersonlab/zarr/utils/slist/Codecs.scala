package org.lasersonlab.zarr.utils.slist

import cats.Foldable
import cats.implicits._
import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json, Printer }
import lasersonlab.shapeless.slist._
import org.lasersonlab.shapeless.SList.FromList
import org.lasersonlab.shapeless.SList.FromList.{ TooFew, TooMany }
import org.lasersonlab.circe.{ CodecK, DecoderK, EncoderK }
import org.lasersonlab.zarr.circe.pprint

trait Codecs {

  implicit val codec_0: CodecK[`0`] =
    new EncoderK[`0`] with DecoderK[`0`] {

      def apply[T](implicit d: Encoder[T]): Encoder[`0`[T]] =
        new Encoder[`0`[T]] {
          override def apply(a: `0`[T]): Json = Json.arr()
        }

      def apply[T](implicit d: Decoder[T]): Decoder[`0`[T]] =
        new Decoder[`0`[T]] {
          override def apply(c: HCursor): Result[`0`[T]] =
            c
              .as[List[T]]
              .flatMap {
                case Nil ⇒
                  Right(⊥)
                case extra ⇒
                  Left(
                    DecodingFailure(
                      s"Found ${extra.size} extra elements: ${pprint(c.value)}",
                      c.history
                    )
                  )
              }
        }
    }

  implicit def makeCodecK[F[_]: Foldable](implicit from: FromList[F]): CodecK[F] =
    new EncoderK[F] with DecoderK[F] {
      def apply[T](implicit d: Encoder[T]): Encoder[F[T]] =
        new Encoder[F[T]] {
          def apply(a: F[T]): Json =
            Json.arr(
              a
                .toList
                .map(d(_))
              : _*
            )
        }

      def apply[T](implicit d: Decoder[T]): Decoder[F[T]] =
        new Decoder[F[T]] {
          def apply(c: HCursor): Result[F[T]] =
            c
              .as[List[T]]
              .flatMap(
                from(_)
                  .left
                  .map {
                    err ⇒
                      DecodingFailure(
                        err match {
                          case TooFew (    by) ⇒ s"Expected ${from.size} elements; found ${from.size -          by}: ${pprint(c.value)}"
                          case TooMany(extras) ⇒ s"Expected ${from.size} elements, found ${from.size + extras.size}: ${pprint(c.value)}"
                        },
                        c.history
                      )
                  }
              )
        }
    }

  implicit def makeEncoder[F[_], T](implicit hkt: EncoderK[F], d: Encoder[T]): Encoder[F[T]] = hkt(d)
  implicit def makeDecoder[F[_], T](implicit hkt: DecoderK[F], d: Decoder[T]): Decoder[F[T]] = hkt(d)
}
object Codecs extends Codecs
