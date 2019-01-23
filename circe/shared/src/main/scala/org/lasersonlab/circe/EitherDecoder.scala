package org.lasersonlab.circe

import io.circe.Decoder

trait EitherDecoder {
  implicit def eitherDecoder[A,B](implicit a: Decoder[A], b: Decoder[B]): Decoder[Either[A,B]] = {
    val l: Decoder[Either[A,B]]= a.map(Left.apply)
    val r: Decoder[Either[A,B]]= b.map(Right.apply)
    l or r
  }
}
