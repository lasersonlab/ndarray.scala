package org.lasersonlab.zarr

import _root_.io.circe.Decoder.Result
import _root_.io.circe._

sealed trait Format
object Format {
  // Zarr version 1 not supported (yet?)
  // case object `1` extends Format

  case object `2` extends Format

  implicit val decoder: Decoder[Format] =
    new Decoder[Format] {
      override def apply(c: HCursor): Result[Format] =
        c
          .value
          .as[Int]
          .flatMap {
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
                  s"Unrecognized Zarr version: $s",
                  c.history
                )
              )
          }
    }

  implicit val encoder: Encoder[Format] =
    new Encoder[Format] {
      def apply(a: Format): Json =
        a match {
          case `2` ⇒ Json.fromInt(2)
        }
    }
}
