package org.lasersonlab.circe

import io.circe.{ Encoder, Json }

trait EncoderK[F[_]] {
  def apply[T](implicit d: Encoder[T]): Encoder[F[T]]
}
object EncoderK {
  trait list extends EncoderK[List] {
    def apply[T](implicit d: Encoder[T]): Encoder[List[T]] =
      new Encoder[List[T]] {
        def apply(a: List[T]): Json = Json.arr(a.map(d(_)): _*)
      }
  }
  implicit object list extends list
}

