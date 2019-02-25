package org.lasersonlab.gcp

case class Metadata(
  id: String,
  name: String,
  size: Long,
  md5Hash: String
)

object Metadata {
  import io.circe.generic.semiauto._
  implicit val decoder = deriveDecoder[Metadata]
}
