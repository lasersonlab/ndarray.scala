package org.lasersonlab.gcp.googleapis

import io.circe.{ Decoder, Encoder }

case class Paged[T](
          items: Vector[T],
  nextPageToken: ?[String] = None
) {
  def +(next: Paged[T]): Paged[T] =
    copy(
      items ++ next.items,
      next.nextPageToken
    )
}

object Paged extends PagedCodecs {
  implicit def unwrap[T](paged: Paged[T]): Vector[T] = paged.items
}
trait PagedCodecs {
  import io.circe.generic.semiauto._
  implicit def pagedDecoder[T: Decoder]: Decoder[Paged[T]] = deriveDecoder[Paged[T]]
  implicit def pagedEncoder[T: Encoder]: Encoder[Paged[T]] = deriveEncoder[Paged[T]]
}
