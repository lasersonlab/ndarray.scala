package org.lasersonlab.circe

import io.circe.Encoder
import io.circe.syntax._

trait EitherEncoder {
  implicit def eitherEncoder[A: Encoder, B: Encoder]: Encoder[Either[A, B]] = {
    case Left(a) ⇒ a.asJson
    case Right(b) ⇒ b.asJson
  }
}
