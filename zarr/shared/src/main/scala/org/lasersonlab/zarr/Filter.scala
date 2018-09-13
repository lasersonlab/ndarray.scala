package org.lasersonlab.zarr

import circe.Decoder.Result
import circe._
import shapeless.{ CNil, Generic }

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
  implicit val encoder: Encoder[Filter] =
    new Encoder[Filter] {
      override def apply(f: Filter): Json = ???
    }

  /**
   * Placeholder while [[Filter]] remains uninhabited; lets downstream derivations work
   */
  implicit val generic: Generic.Aux[Filter, CNil] =
    new Generic[Filter] {
      type Repr = CNil
      override def   to(t: Filter):   CNil = ???
      override def from(r:   CNil): Filter = ???
    }
}
