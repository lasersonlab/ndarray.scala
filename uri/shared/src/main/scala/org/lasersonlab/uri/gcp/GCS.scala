package org.lasersonlab.uri.gcp

import java.net.{ URI, URLEncoder }

import cats.effect._
import cats.implicits._
import org.lasersonlab.uri.{ Config, encode, Http, Uri, http ⇒ h }

case class Metadata(
  id: String,
  name: String,
  size: Long,
  md5Hash: String
)

case class GCS[F[_]: ConcurrentEffect](
  bucket: String,
  path: Vector[String]
)(
  implicit
  auth: Auth,
  val config: Config
)
extends Uri[F] {

  //val encPath = encode(path.mkString("/"))

  val uri =
    if (path.isEmpty)
      new URI(s"gs://$bucket")
    else
      new URI(s"gs://$bucket/${path.mkString("/")}")

  import com.softwaremill.sttp._

  implicit val reqConfig =
    h.Config(
      headers = Map("Authorization" → s"Bearer ${auth.token}")
    )

//  import scala.concurrent.ExecutionContext.Implicits.global

  val u = uri"https://www.googleapis.com/storage/v1/b/$bucket/o/${path.mkString("/")}?userProject=${auth.project}"

  override def exists: F[Boolean] = metadata.attempt.map { _.isRight }

  override def parentOpt: Option[GCS[F]] =
    path match {
      case Vector() ⇒ None
      case parent :+ _ ⇒ Some(GCS(bucket, parent))
    }

  lazy val metadata: F[Metadata] = {
    val http = Http(u.toJavaUri)
    import io.circe.generic.auto._
    http.json[Metadata]
  }

  lazy val size: F[Long] = metadata.map(_.size)

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    Http(u.param("alt", "media").toJavaUri).bytes(start, size)
}
