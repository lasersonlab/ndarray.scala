package org.lasersonlab

import cats.MonadError
import hammerlab.option.Opt
import _root_.io.circe.syntax.EncoderOps
import _root_.io.circe.{ Decoder, Encoder }

package object uri
extends uri.syntax
   with lasersonlab.future {
  type MonadErr[F[_]] = MonadError[F, Throwable]
  type ?[+T] = Opt[T]

  implicit def decodeOpt[T](implicit d: Decoder[T]): Decoder[?[T]] = Decoder.decodeOption[T].map { t ⇒ t: ?[T] }
  implicit def encodeOpt[T](implicit d: Encoder[Option[T]]): Encoder[?[T]] = (a: ?[T]) => (a: Option[T]).asJson
}
