package org.lasersonlab.circe

import io.circe.{ Decoder, Encoder, HCursor }
import shapeless._

trait SingletonEncoders {
  implicit def consEncoder[E](implicit e: Encoder[E]): Encoder[E :: HNil] = { case h :: HNil ⇒ e(h) }
  implicit def ccEncoder[T, L <: HList](implicit g: Generic.Aux[T, L], e: Encoder[L]): Encoder[T] = (t: T) ⇒ e(g.to(t))
}
trait SingletonDecoders {
  implicit def consDecoder[E](implicit d: Decoder[E]): Decoder[E :: HNil] = d(_).map { _ :: HNil }
  implicit def ccDecoder[T, L <: HList](implicit g: Generic.Aux[T, L], d: Decoder[L]): Decoder[T] = d(_).map(g.from)
}
trait SingletonCodec
  extends SingletonEncoders
     with SingletonDecoders
object SingletonCodec extends SingletonCodec
