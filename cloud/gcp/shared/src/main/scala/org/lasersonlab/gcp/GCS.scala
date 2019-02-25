package org.lasersonlab.gcp

import java.net.URI

import cats.implicits._
import org.lasersonlab.gcp.Config.implicits._
import org.lasersonlab.gcp.googleapis.projects.UserProject
import org.lasersonlab.gcp.googleapis.storage
import org.lasersonlab.gcp.googleapis.storage.{ Objects, Prefix }
import org.lasersonlab.gcp.oauth.Auth
import org.lasersonlab.uri.Uri.Segment
import org.lasersonlab.uri.{ Http, Uri, caching, http ⇒ h }

case class GCS(
  bucket: String,
  path: Vector[String]
)(
  implicit
  config: Config,
  val userProject: Option[UserProject],
  val cachingConfig: caching.Config,
)
extends Uri()(config.httpConfig)
   with Prefix {

  type Self = GCS

  val uri =
    if (path.isEmpty)
      new URI(s"gs://$bucket")
    else
      new URI(s"gs://$bucket/${path.mkString("/")}")

  import com.softwaremill.sttp._

  def objectUrl = uri"${storage.base}/b/$bucket/o/${path.mkString("/")}?userProject=$userProject"
  def listUri = uri"${storage.base}/b/$bucket/o?delimiter=${"/"}&prefix=${path.mkString("", "/", "/")}&userProject=$userProject"

  override def /(name: String): Self = GCS(bucket, path :+ name)

  override def exists: F[Boolean] = metadata.attempt.map { _.isRight }

  override def isDirectory: Boolean = uri.toString.endsWith("/")

  override def children: F[Iterator[Self]] = {
    Http(listUri.toJavaUri)
      .json[Objects]
      .map {
        case l @ Objects(prefixes, items, _) ⇒
          items
            .fold {
              Iterator[Metadata]()
            } {
              _.iterator
            }
            .map {
              md ⇒
                GCS(bucket, path :+ md.name)
            } ++
          prefixes
            .fold(Iterator[String]()) {
              _.iterator
            }
            .map {
              name ⇒
                GCS(
                  bucket,
                  path :+ name
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
    project: Option[UserProject],
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
