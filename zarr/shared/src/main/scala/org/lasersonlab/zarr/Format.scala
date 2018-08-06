package org.lasersonlab.zarr

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }

sealed trait Format
object Format {
  // Zarr version 1 not supported (yet?)
  // case object `1` extends Format

  case object `2` extends Format

  implicit val decoder: Decoder[Format] =
    new Decoder[Format] {
      override def apply(c: HCursor): Result[Format] =
        c.value.as[Int].flatMap {
          case 2 ⇒ Right(`2`)
          case 1 ⇒
            Left(
              DecodingFailure(
                s"Zarr version 1 not supported",
                c.history
              )
            )
          case s ⇒
            Left(
              DecodingFailure(
                s"Unrecognized format: $s",
                c.history
              )
            )
        }
    }
}
