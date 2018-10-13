package org.lasersonlab.zarr

import cats.Eq
import org.lasersonlab.zarr.circe.Decoder.Result
import org.lasersonlab.zarr.circe._

// TODO: add filters
sealed trait Filter
object Filter {
  implicit val decoder: Decoder[Filter] =
    new Decoder[Filter] {
      override def apply(c: HCursor): Result[Filter] =
        Left(
          DecodingFailure(
            s"Filters not supported:\n${c.value.spaces2}",
            c.history
          )
        )
    }

  implicit val encoder: Encoder[Filter] = new Encoder[Filter] { def apply(f: Filter): Json = ??? }

  implicit val _eq: Eq[Filter] = new Eq[Filter] { def eqv(x: Filter, y: Filter): Boolean = ??? }
}
