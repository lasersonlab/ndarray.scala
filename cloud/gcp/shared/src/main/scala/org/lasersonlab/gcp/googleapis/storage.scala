package org.lasersonlab.gcp.googleapis

import io.circe.generic.auto._
import org.lasersonlab.gcp.googleapis.projects.{ Project, UserProject }
import org.lasersonlab.gcp.{ Config, Metadata, googleapis }
import org.lasersonlab.uri._
import org.lasersonlab.gcp._
import com.softwaremill.sttp._

object storage {
  val base = uri"${googleapis.base}/storage/v1"

  case class Objects(
         prefixes: ?[Vector[  String]] = None,
            items: ?[Vector[Metadata]] = None,
    nextPageToken: ?[         String ] = None
  ) {
    def dirs = prefixes.getOrElse(Vector())
    def files = items.getOrElse(Vector())
  }
  object Objects extends Kinded("storage#objects") {
    implicit val decoder = kindDecoder[Objects]
    implicit val encoder = kindEncoder[Objects]
  }

  case class Billing(requesterPays: Boolean = false)
  case class Bucket(
    id: String,
    name: String,
    projectNumber: Long,
    billing: ?[Billing] = None,
    objects: ?[Objects] = None
  ) {
    def uri = uri"${storage.base}/b/$name"
    //override def toString: String = name
    def ls(path: String*)(
      implicit
      config: Config,
      userProject: ?[UserProject] = None
    ):
      ?[F[Δ[Bucket]]] =
    {
      objects
        .fold {
          Option(
            Http(
              uri"$uri/o?delimiter=${"/"}&prefix=${path.mkString("/")}&userProject=$userProject"
            )
            .json[Objects]
            .map {
              objects ⇒
                (bucket: Bucket) ⇒ bucket.copy(objects = Some(objects))
            }
          )
        } {
          _ ⇒ None
        }
    }
  }
  object Bucket extends Kinded("storage#bucket") {
    implicit val decoder = kindDecoder[Bucket]
    implicit val encoder = kindEncoder[Bucket]
  }

  // TODO: this can probably be removed if we use a more permissive [[Decoder]] for [[Paged]], which allows the `items`
  //  [[Vector]] to be absent
  case class Buckets(
    items: ?[Vector[Bucket]],
    nextPageToken: ?[String] = None
  ) {
    def buckets = items.getOrElse(Vector())
  }
  object Buckets extends Kinded("storage#buckets") {
    implicit val decoder = kindDecoder[Buckets]
    implicit val encoder = kindEncoder[Buckets]
    implicit def unwrap(buckets: Buckets): Vector[Bucket] = buckets.buckets
  }

  def buckets()(implicit config: Config, project: Project): F[Paged[Bucket]] = {
    println(s"requesting buckets for project $project")
    Http(uri"https://www.googleapis.com/storage/v1/b?project=${project.id}".toJavaUri)
      .json[Buckets]
      .map {
        case Buckets(items, nextPageToken) ⇒
               Paged(items.getOrElse(Vector()), nextPageToken)
      }
  }
}
