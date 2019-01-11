package org.lasersonlab.uri.gcp

import java.net.URI

import cats.implicits._
import org.lasersonlab.uri.Uri.Segment
import org.lasersonlab.uri.{ Config, Http, Uri, http ⇒ h }

import scala.concurrent.ExecutionContext

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

case class GCS(
  bucket: String,
  path: Vector[String]
)(
  implicit
  auth: Auth,
  val project: Option[Project],
  val config: Config,
  httpConfig: h.Config
)
extends Uri()(httpConfig) {

  type Self = GCS

  val uri =
    if (path.isEmpty)
      new URI(s"gs://$bucket")
    else
      new URI(s"gs://$bucket/${path.mkString("/")}")

  import com.softwaremill.sttp._
  
  def objectUrl = uri"https://www.googleapis.com/storage/v1/b/$bucket/o/${path.mkString("/")}?userProject=$project"
  def listUri = uri"https://www.googleapis.com/storage/v1/b/$bucket/o?delimiter=${"/"}&prefix=${path.mkString("", "/", "/")}&userProject=$project"

  override def /(name: String): Self = GCS(bucket, path :+ name)

  override def exists: F[Boolean] = metadata.attempt.map { _.isRight }

  import googleapis.storage
  override def list: F[Iterator[Self]] = {
    Http(listUri.toJavaUri)
      .json[storage.Objects]
      .map {
        case l @ storage.Objects(dirs, items, _) ⇒
          items
            .fold(Iterator[Metadata]())(_.iterator)
            .map {
              m ⇒
                GCS(bucket, m.name.split("/").toVector)
            } ++
          dirs
            .getOrElse(Nil)
            .map {
              path ⇒
                GCS(
                  bucket,
                  path
                    .split("/")
                    .toVector
                )
            }
      }
  }

  override def parentOpt: Option[Self] =
    path match {
      case Vector() ⇒ None
      case parent :+ _ ⇒ Some(GCS(bucket, parent))
    }

  lazy val metadata: F[Metadata] = {
    val http = Http(objectUrl.toJavaUri)
    import io.circe.generic.auto._
    http.json[Metadata]
  }

  override lazy val size: F[Long] = metadata.map(_.size)

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    Http(objectUrl.param("alt", "media").toJavaUri).bytes(start, size)
}

object GCS {
  case class Arg(override val toString: String) { def apply() = toString }
  object Arg {
    implicit def fromSegment[T](t: T)(implicit s: Segment[T]): Arg = Arg(s(t))
    implicit def unwrap(arg: Arg): String = arg.toString
  }
  def apply(
    bucket: Arg,
    path: Arg*
  )(
    implicit
    auth: Auth,
    project: Option[Project],
    config: Config,
    httpConfig: h.Config
  ):
    GCS =
    GCS(
      bucket,
      path
        .map(_())
        .toVector
    )
}
