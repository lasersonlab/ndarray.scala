package org.lasersonlab

import io.circe.Decoder.Result
import io.circe.{ Decoder, Encoder, HCursor, Json }

package object circe {
  trait DecoderK[F[_]] {
    def apply[T](implicit d: Decoder[T]): Decoder[F[T]]
  }
  object DecoderK {
    implicit val list: DecoderK[List] =
      new DecoderK[List] {
        def apply[T](implicit d: Decoder[T]): Decoder[List[T]] =
          new Decoder[List[T]] {
            def apply(c: HCursor): Result[List[T]] =
              c.as[List[T]]
          }
      }
  }

  trait EncoderK[F[_]] {
    def apply[T](implicit d: Encoder[T]): Encoder[F[T]]
  }
  object EncoderK {
    implicit val list: EncoderK[List] =
      new EncoderK[List] {
        def apply[T](implicit d: Encoder[T]): Encoder[List[T]] =
          new Encoder[List[T]] {
            def apply(a: List[T]): Json = Json.arr(a.map(d(_)): _*)
          }
      }
  }

  type CodecK[F[_]] = EncoderK[F] with DecoderK[F]
  type Codec [T   ] = Encoder [T] with Decoder [T]
}
