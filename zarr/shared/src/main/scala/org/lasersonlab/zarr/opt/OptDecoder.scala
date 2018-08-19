package org.lasersonlab.zarr.opt

import io.circe.{ Decoder, HCursor }
import io.circe.Decoder._
import hammerlab.option._

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
