package org.lasersonlab.circe

import java.time.Instant
import Instant.ofEpochSecond

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json, JsonNumber }
import io.circe.syntax._

trait InstantEncoder {
  implicit val instantEncoder: Encoder[Instant] = _.getEpochSecond.asJson
}
trait InstantDecoder {
  implicit val instantDecoder: Decoder[Instant] =
    (c: HCursor) ⇒
      c
        .value
        .as[JsonNumber]
        .flatMap {
          number ⇒
            number
            .toLong
            .fold[Result[Instant]] {
              Left(
                DecodingFailure(
                  s"Invalid epoch seconds: $number",
                  c.history
                )
              )
            } {
              seconds ⇒
                Right(
                  ofEpochSecond(
                    seconds
                  )
                )
            }
        }
}
trait InstantCodec
extends InstantEncoder
   with InstantDecoder
