package org.lasersonlab.uri.gcp

import java.net.URI

import cats.effect._
import cats.implicits._
import org.lasersonlab.uri.Uri.Segment
import org.lasersonlab.uri.{ Config, Http, Uri, http ⇒ h }

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

case class GCS[F[_]: ConcurrentEffect](
  bucket: String,
  path: Vector[String]
)(
  implicit
  auth: Auth,
  val config: Config
)
extends Uri[F] {

  type Self = GCS[F]

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


  val objectUrl = uri"https://www.googleapis.com/storage/v1/b/$bucket/o/${path.mkString("/")}?userProject=${auth.project}"
  def listUri = uri"https://www.googleapis.com/storage/v1/b/$bucket/o?delimiter=${"/"}&prefix=${path.mkString("", "/", "/")}&userProject=${auth.project}"

  override def /(name: String): GCS[F] = GCS(bucket, path :+ name)

  override def exists: F[Boolean] = metadata.attempt.map { _.isRight }

  import googleapis.storage
  override def list: F[List[GCS[F]]] = {
    Http(listUri.toJavaUri)
      .json[storage.Objects]
      .map {
        case l @ storage.Objects(dirs, items, _) ⇒
          items
            .fold[List[Metadata]](Nil)(_.toList)
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

  override def parentOpt: Option[GCS[F]] =
    path match {
      case Vector() ⇒ None
      case parent :+ _ ⇒ Some(GCS(bucket, parent))
    }

  lazy val metadata: F[Metadata] = {
    val http = Http(objectUrl.toJavaUri)
    import io.circe.generic.auto._
    http.json[Metadata]
  }

  lazy val size: F[Long] = metadata.map(_.size)

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    Http(objectUrl.param("alt", "media").toJavaUri).bytes(start, size)
}

object GCS {
  case class Arg(override val toString: String) { def apply() = toString }
  object Arg {
    implicit def fromSegment[T](t: T)(implicit s: Segment[T]): Arg = Arg(s(t))
    implicit def unwrap(arg: Arg): String = arg.toString
  }
  def apply[
    F[_]: ConcurrentEffect
  ](
    bucket: Arg,
    path: Arg*
  )(
    implicit
    auth: Auth,
    config: Config
  ):
    GCS[F] =
    GCS(
      bucket,
      path
        .map(_())
        .toVector
    )
}
