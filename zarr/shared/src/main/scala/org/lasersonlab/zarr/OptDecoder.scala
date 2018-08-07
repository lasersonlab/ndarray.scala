package org.lasersonlab.zarr

import io.circe.{ Decoder, HCursor }
import io.circe.Decoder.Result
import hammerlab.option._

trait OptDecoder {
  implicit def optDecoder[T: Decoder]: Decoder[Opt[T]] =
    new Decoder[Opt[T]] {
      override def apply(c: HCursor): Result[Opt[T]] =
        Decoder
          .decodeOption[T]
          .apply(c)
          .map { o ⇒ o: Opt[T] }
    }
}

object OptDecoder extends OptDecoder
