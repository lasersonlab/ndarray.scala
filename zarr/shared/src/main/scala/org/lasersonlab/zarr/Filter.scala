package org.lasersonlab.zarr

import _root_.io.circe.Decoder.Result
import _root_.io.circe._

// TODO: add filters
sealed trait Filter
object Filter {
  implicit val decoder: Decoder[Filter] =
    new Decoder[Filter] {
      override def apply(c: HCursor): Result[Filter] =
        Left(
          DecodingFailure(
            "Filters not supported",
            c.history
          )
        )
    }
  implicit val encoder: Encoder[Filter] =
    new Encoder[Filter] {
      override def apply(f: Filter): Json = Json.Null
    }
}
