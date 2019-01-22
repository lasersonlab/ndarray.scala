package org.lasersonlab.gcp

import io.circe.generic.decoding.DerivedDecoder
import io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json }
import shapeless.Lazy
import com.softwaremill.sttp._
import io.circe.generic.encoding.DerivedObjectEncoder

package object googleapis
  extends Config.implicits
{
  val base = uri"https://www.googleapis.com"

  type ?[+T] = Option[T]

  case class Kind(override val toString: String)

  abstract class Kinded(kind: String) {
    implicit val _kind = Kind(kind)
  }

  def kindDecoder[A](implicit d: Lazy[DerivedDecoder[A]], kind: Kind): Decoder[A] =
    (c: HCursor) ⇒
      c
        .downField("kind")
        .as[String]
        .flatMap {
          case k if k == kind.toString ⇒ d.value(c)
          case k ⇒ Left(DecodingFailure(s"Found kind $k, expected $kind", c.history))
        }

  def kindEncoder[A](implicit e: Lazy[DerivedObjectEncoder[A]], kind: Kind): Encoder[A] =
    (a: A) ⇒
      Json.fromJsonObject(
        e
          .value
          .encodeObject(a)
          .+:("kind" → Json.fromString(kind.toString))
      )
}
