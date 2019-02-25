package org.lasersonlab

import io.circe.Decoder.Result
import io.circe.{ Decoder, Encoder, HCursor, Json }

package object circe {
  trait codec {
    implicit object list
      extends DecoderK.list
         with EncoderK.list

    implicit def wrap[T](implicit d: Decoder[T], e: Encoder[T]): Codec[T] =
      new Encoder[T] with Decoder[T] {
        @inline def apply(a: T): Json = e(a)
        @inline def apply(c: HCursor): Result[T] = d(c)
      }
  }
  object codec extends codec

  type CodecK[F[_]] = EncoderK[F] with DecoderK[F]
  type Codec [T   ] = Encoder [T] with Decoder [T]
}
