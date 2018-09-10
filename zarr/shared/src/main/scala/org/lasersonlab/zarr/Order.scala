package org.lasersonlab.zarr

import _root_.io.circe.Decoder.Result
import _root_.io.circe._

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

  implicit val encoder: Encoder[Order] =
    new Encoder[Order] {
      def apply(a: Order): Json =
        a match {
          case C ⇒ Json.fromString("C")
        }
    }

  // TODO: column-major order not implemented (yet?)
//  case object F extends Order
}
