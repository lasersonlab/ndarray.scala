package org.lasersonlab.zarr.utils.opt

import cats.implicits._

import hammerlab.option._
import io.circe.Decoder._
import io.circe.Encoder._
import io.circe._

trait OptDecoder {
  implicit def optDecoder[T: Decoder]: Decoder[Opt[T]] =
    new Decoder[Opt[T]] {
      override def apply(c: HCursor): Result[Opt[T]] =
        decodeOption[T]
          .apply(c)
          .map { o ⇒ o }
    }
}

object OptDecoder extends OptDecoder

trait OptEncoder {
  implicit def optEncoder[T: Encoder]: Encoder[Opt[T]] =
    new Encoder[Opt[T]] {
      override def apply(a: Opt[T]): Json =
        encodeOption[T].apply(a)
    }
}

object OptEncoder extends OptEncoder

trait OptCodec
  extends OptDecoder
     with OptEncoder

object OptCodec extends OptCodec
