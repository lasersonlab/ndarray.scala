package org.lasersonlab.uri.gcp

import java.net.URL

import cats.implicits._
import com.softwaremill.sttp._
import hammerlab.option.Opt
import io.circe.Decoder.Result
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.auto._
import io.circe.{ Decoder, DecodingFailure, HCursor }
import lasersonlab.future.F
import org.lasersonlab.uri.gcp.googleapis.projects.{ Project }
import org.lasersonlab.uri.gcp.googleapis.storage.{ Bucket }
import org.lasersonlab.uri.{ Http, http }
import shapeless.Lazy

object googleapis {
  val base = uri"https://www.googleapis.com"

  type ?[+T] = Opt[T]

  def kindDecoder[A](kind: String)(implicit d: Lazy[DerivedDecoder[A]]): Decoder[A] =
    new Decoder[A] {
      override def apply(c: HCursor): Result[A] =
        c
          .downField("kind")
          .as[String]
            .flatMap {
              case k if k == kind ⇒ d.value(c)
              case k ⇒ Left(DecodingFailure(s"Found kind $k, expected $kind", c.history))
            }
    }

  case class User(
    id: String,
    name: String,
    email: String,
    img: String
  )

  object userinfo {
    def apply()(implicit auth: Auth, httpConfig: http.Config): F[User] = {
      Http(uri"https://www.googleapis.com/oauth2/v1/userinfo?alt=json".toJavaUri)
        .json[User]
    }
  }

  case class Paged[T](
            items: Vector[T],
    nextPageToken: ?[String] = None
  )
  object Paged {
    implicit def unwrap[T](paged: Paged[T]): Vector[T] = paged.items
  }

  import http.Config._

  object projects {
    val url = uri"https://cloudresourcemanager.googleapis.com/v1/projects".toJavaUri
    case class Projects(projects: Option[Vector[Project]] = None, nextPageToken: Option[String] = None)

    import io.circe.generic.extras._
    implicit val config: Configuration = Configuration.default
    @ConfiguredJsonCodec
    case class Project(
      @JsonKey(         "name")   name:    String,
      @JsonKey(    "projectId")     id:    String,
      @JsonKey("projectNumber") number:    String,
                               buckets: ?[Paged[Bucket]]
    ) {
      def fetchBuckets(implicit auth: Auth, h: http.Config): F[Project] =
        if (buckets.isEmpty) {
          implicit val project = this
          googleapis
            .storage
            .buckets
            .map {
              buckets ⇒
                copy(
                  buckets = buckets
                )
            }
        } else
          F { this }
    }

    case class UserProject(project: Project) {
      override def toString: String = project.id
    }
    object UserProject {
      implicit def   wrap(    project:     Project): UserProject = UserProject(project)
      implicit def unwrap(userProject: UserProject):     Project = userProject.project
    }

    def apply()(implicit auth: Auth, httpConfig: http.Config): F[Paged[Project]] = {
      Http(url)
        .json[Projects]
        .map {
          case Projects(items, nextPageToken) ⇒
            Paged(items.getOrElse(Vector()), nextPageToken)
        }
    }
  }

  object storage {
    val base = uri"${googleapis.base}/storage/v1"

    case class Objects(
           prefixes: ?[Seq[  String]] = None,
              items: ?[Seq[Metadata]] = None,
      nextPageToken: ?[      String ] = None
    )
    object Objects {
      implicit val decoder = kindDecoder[Objects]("storage#objects")
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
    object Bucket {
      implicit val decoder = kindDecoder[Bucket]("storage#bucket")
    }

    case class Buckets(
      items: ?[Vector[Bucket]],
      nextPageToken: ?[String] = None
    ) {
      def buckets = items.getOrElse(Vector())
    }
    object Buckets {
      implicit val decoder = kindDecoder[Buckets]("storage#buckets")
      implicit def toSeq(buckets: Buckets): Vector[Bucket] = buckets.buckets

      case class Config(
               auth:          Auth,
            project:       Project,
      )
      object Config {
        implicit def wrap(
          implicit
                 auth:          Auth,
              project:       Project,
        ): Config =
           Config(auth, project)
      }
    }

    def buckets(implicit config: Buckets.Config, httpConfig: http.Config): F[Paged[Bucket]] = {
      implicit val Buckets.Config(auth, project) = config
      println(s"requesting buckets for project $project")
      Http(uri"https://www.googleapis.com/storage/v1/b?project=${project.id}".toJavaUri)
        .json[Buckets]
        .map {
          case Buckets(items, nextPageToken) ⇒
                 Paged(items.getOrElse(Vector()), nextPageToken)
        }
    }
  }
}
