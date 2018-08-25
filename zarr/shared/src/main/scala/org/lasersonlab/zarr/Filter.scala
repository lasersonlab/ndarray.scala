package org.lasersonlab.zarr

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json }

// TODO: filter support
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
