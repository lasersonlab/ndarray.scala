package org.lasersonlab

import io.circe.Decoder.Result
import io.circe.{ Decoder, Encoder, HCursor, Json }

package object circe {
  trait DecoderK[F[_]] {
    def apply[T](implicit d: Decoder[T]): Decoder[F[T]]
  }
  object DecoderK {
    trait list extends DecoderK[List] {
      def apply[T](implicit d: Decoder[T]): Decoder[List[T]] =
        new Decoder[List[T]] {
          def apply(c: HCursor): Result[List[T]] =
            c.as[List[T]]
        }
    }
    implicit object list extends list
  }

  trait EncoderK[F[_]] {
    def apply[T](implicit d: Encoder[T]): Encoder[F[T]]
  }
  object EncoderK {
    trait list extends EncoderK[List] {
      def apply[T](implicit d: Encoder[T]): Encoder[List[T]] =
        new Encoder[List[T]] {
          def apply(a: List[T]): Json = Json.arr(a.map(d(_)): _*)
        }
    }
    implicit object list extends list
  }

  object codec {
    implicit object list
      extends DecoderK.list
         with EncoderK.list

    implicit def wrap[T](implicit d: Decoder[T], e: Encoder[T]): Codec[T] =
      new Encoder[T] with Decoder[T] {
        @inline def apply(a: T): Json = e(a)
        @inline def apply(c: HCursor): Result[T] = d(c)
      }
  }

  type CodecK[F[_]] = EncoderK[F] with DecoderK[F]
  type Codec [T   ] = Encoder [T] with Decoder [T]
}
