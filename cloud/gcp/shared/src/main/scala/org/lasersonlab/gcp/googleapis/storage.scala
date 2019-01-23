package org.lasersonlab.gcp.googleapis

import io.circe.generic.auto._
import io.circe.syntax._
import org.lasersonlab.gcp.googleapis.projects.{ Project, UserProject }
import org.lasersonlab.gcp.{ Config, Metadata, googleapis }
import org.lasersonlab.uri._
import org.lasersonlab.gcp._
import com.softwaremill.sttp._
import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject }

object storage {
  val base = uri"${googleapis.base}/storage/v1"

  object Path {
    def unapply(str: String): Option[(String, Vector[String])] =
      str
        .split("/") match {
          case Array(bucket, rest @ _*) ⇒
            Some(
              bucket,
              rest.toVector
            )
          case _ ⇒ None
        }
  }

  case class Dir(
    bucket: String,
    path: Vector[String],
    objects: ?[Objects] = None
  ) {
    def name = (bucket +: path) mkString("", "/", "/")
  }
  object Dir {
    /** Simplified JSON representation (when [[Objects]] is populated; flat [[String name string]] otherwise */
    case class Repr(name: String, objects: Objects)
    implicit val encoder: Encoder[Dir] = {
      case dir @ Dir(bucket, path, None         ) ⇒ dir.name.asJson
      case dir @ Dir(bucket, path, Some(objects)) ⇒ Repr(dir.name, objects).asJson
    }
    import lasersonlab.circe._
    implicit val decoder: Decoder[Dir] =
      (c: HCursor) ⇒
        c
          .value
          .as[Either[String, Repr]]
          .flatMap {
            case
              Left(
                Path(
                  bucket,
                  path
                )
              ) ⇒
              Right(
                Dir(
                  bucket,
                  path
                )
              )
            case
              Right(
                Repr(
                  Path(
                    bucket,
                    path
                  ),
                  objects
                )
              ) ⇒
              Right(
                Dir(
                  bucket,
                  path,
                  Some(objects)
                )
              )
            case other ⇒
              Left(
                DecodingFailure(
                  s"Bad dir objects: $other",
                  c.history
                )
              )
          }
  }
  case class Obj(bucket: String, path: Vector[String], metadata: Metadata)
  object Obj {
    implicit val encoder: Encoder[Obj] = { case Obj(_, _, metadata) ⇒ metadata.asJson }
    implicit val decoder: Decoder[Obj] =
      (c: HCursor) ⇒
        c
          .value
          .as[Metadata]
          .flatMap {
            metadata ⇒
              metadata.name match {
                case Path(bucket, path) ⇒
                  Right(
                    Obj(
                      bucket,
                      path,
                      metadata
                    )
                  )
                case name ⇒
                  Left(
                    DecodingFailure(
                      s"Invalid object name $name",
                      c.history
                    )
                  )
              }
          }
  }

  case class Objects(
         prefixes: ?[Vector[Dir]] = None,
            items: ?[Vector[Obj]] = None,
    nextPageToken: ?[    String ] = None
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
