package org.lasersonlab.circe

import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax._

trait EitherDecoder {
  implicit def eitherDecoder[A,B](implicit a: Decoder[A], b: Decoder[B]): Decoder[Either[A,B]] = {
    val l: Decoder[Either[A,B]]= a.map(Left.apply)
    val r: Decoder[Either[A,B]]= b.map(Right.apply)
    l or r
  }
}

trait EitherEncoder {
  implicit def eitherEncoder[A: Encoder, B: Encoder]: Encoder[Either[A, B]] = {
    case Left(a) ⇒ a.asJson
    case Right(b) ⇒ b.asJson
  }
}

  trait EitherCodec
extends EitherEncoder
   with EitherDecoder
