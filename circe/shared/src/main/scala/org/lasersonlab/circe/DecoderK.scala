package org.lasersonlab.circe

import io.circe.Decoder.Result
import io.circe.{ Decoder, HCursor }

trait DecoderK[F[_]] {
  def apply[T](implicit d: Decoder[T]): Decoder[F[T]]
}
object DecoderK {
  trait list extends DecoderK[List] {
    def apply[T](implicit d: Decoder[T]): Decoder[List[T]] =
      new Decoder[List[T]] {
        def apply(c: HCursor): Result[List[T]] =
          c.as[List[T]]
      }
  }
  implicit object list extends list
}

