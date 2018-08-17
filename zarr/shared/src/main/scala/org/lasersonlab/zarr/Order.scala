package org.lasersonlab.zarr

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }

/**
 * Either “C” or “F”, defining the layout of bytes within each chunk of the array.
 *
 * - “C” means row-major order, i.e., the last dimension varies fastest
 * - “F” means column-major order, i.e., the first dimension varies fastest
 */
sealed trait Order
object Order {
  case object C extends Order

  implicit val decoder: Decoder[Order] =
    new Decoder[Order] {
      def apply(c: HCursor): Result[Order] =
        c
          .value
          .as[String]
          .flatMap {
            case "C" ⇒ Right(C)
            case o ⇒
              Left(
                DecodingFailure(
                  s"Unexpected order: $o",
                  c.history
                )
              )
          }
    }

  // TODO: column-major order not implemented (yet?)
//  case object F extends Order
}
