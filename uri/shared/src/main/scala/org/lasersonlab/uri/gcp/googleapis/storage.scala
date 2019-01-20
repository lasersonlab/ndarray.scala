package org.lasersonlab.uri.gcp.googleapis

import io.circe.generic.auto._
import org.lasersonlab.uri.gcp.googleapis.projects.Project
import org.lasersonlab.uri.gcp.{ Config, Metadata, googleapis }
import org.lasersonlab.uri._
import org.lasersonlab.uri.gcp._
import com.softwaremill.sttp._

object storage {
  val base = uri"${googleapis.base}/storage/v1"

  case class Objects(
         prefixes: ?[Seq[  String]] = None,
            items: ?[Seq[Metadata]] = None,
    nextPageToken: ?[      String ] = None
  )
  object Objects extends Kinded("storage#objects") {
    implicit val decoder = kindDecoder[Objects]
    implicit val encoder = kindEncoder[Bucket]
  }

  case class Billing(requesterPays: Boolean = false)
  case class Bucket(
    id: String,
    name: String,
    projectNumber: Long,
    billing: ?[Billing] = None
  ) {
    override def toString: String = name
  }
  object Bucket extends Kinded("storage#bucket") {
    implicit val decoder = kindDecoder[Bucket]
    implicit val encoder = kindEncoder[Bucket]
  }

  case class Buckets(
    items: ?[Vector[Bucket]],
    nextPageToken: ?[String] = None
  ) {
    def buckets = items.getOrElse(Vector())
  }
  object Buckets extends Kinded("storage#buckets") {
    implicit val decoder = kindDecoder[Buckets]
    implicit val encoder = kindEncoder[Bucket]
    implicit def toSeq(buckets: Buckets): Vector[Bucket] = buckets.buckets
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
