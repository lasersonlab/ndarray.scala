package org.lasersonlab.uri.gcp

import java.net.URI

import cats.effect._
import cats.implicits._
import org.lasersonlab.uri.{ Config, Uri, http ⇒ h, Http }

case class Metadata(
  id: String,
  name: String,
  size: Long,
  md5Hash: String
)

case class GCS[F[_]: ConcurrentEffect](
  bucket: String,
  path: Vector[String],
  uri: URI
)(
  implicit
  auth: Auth,
  val config: Config
)
extends Uri[F] {

  import com.softwaremill.sttp._

  implicit val reqConfig =
    h.Config(
      headers = Map("Authorization" → s"Bearer: ${auth.token}")
    )

  import scala.concurrent.ExecutionContext.Implicits.global

  val u = uri"https://www.googleapis.com/storage/v1/b/$bucket/o/$path?userProject=${auth.project}"

  override def exists: F[Boolean] = metadata.attempt.map { _.isRight }

  lazy val metadata: F[Metadata] = {
    val http = Http(u.toJavaUri)
    import io.circe.generic.auto._
    http.json[Metadata]
  }

  lazy val size: F[Long] = metadata.map(_.size)

  override def bytes(start: Long, size: Int): F[Array[Byte]] = {
    implicit val reqConfig =
      h.Config(
        headers =
          Map(
            "Authorization" → s"Bearer: ${auth.token}",
            "Range" → s"bytes=$start-${start+size-1}"
          )
      )
    Http(u.param("alt", "media").toJavaUri).read
  }
}
