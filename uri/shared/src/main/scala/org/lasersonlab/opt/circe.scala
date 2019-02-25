package org.lasersonlab.opt

import hammerlab.opt._, std._
import io.circe.Decoder.Result
import io.circe.{ Decoder, Encoder, HCursor }
import _root_.io.circe.syntax.EncoderOps

trait circe {
  implicit def decodeOpt[T](implicit d: Decoder[T]): Decoder[?[T]] =
    new Decoder[?[T]] {
      override def apply(c: HCursor): Result[?[T]] =
        if (c.value.isNull)
          Right(Non)
        else
          d(c).map(Som(_))
    }
  implicit def encodeOpt[T](implicit d: Encoder[Option[T]]): Encoder[?[T]] = (a: ?[T]) => (a: Option[T]).asJson
}
