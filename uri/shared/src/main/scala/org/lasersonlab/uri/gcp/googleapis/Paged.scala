package org.lasersonlab.uri.gcp.googleapis

import io.circe.{ Decoder, Encoder }

case class Paged[T](
          items: Vector[T],
  nextPageToken: ?[String] = None
)

object Paged extends PagedCodecs {
  implicit def unwrap[T](paged: Paged[T]): Vector[T] = paged.items
}
trait PagedCodecs {
  import io.circe.generic.semiauto._
  implicit def pagedDecoder[T: Decoder]: Decoder[Paged[T]] = deriveDecoder[Paged[T]]
  implicit def pagedEncoder[T: Encoder]: Encoder[Paged[T]] = deriveEncoder[Paged[T]]
}
