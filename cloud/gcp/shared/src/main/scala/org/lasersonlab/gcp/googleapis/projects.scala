package org.lasersonlab.gcp.googleapis

import com.softwaremill.sttp._
import io.circe.generic.auto._
import io.circe.generic.extras._
import org.lasersonlab.uri._
import org.lasersonlab.gcp._
import org.lasersonlab.gcp.googleapis.storage.Bucket

// Need this to take precedence over Encoder.encodeIterable
import org.lasersonlab.gcp.googleapis.Paged.pagedEncoder

object projects {
  val url = uri"https://cloudresourcemanager.googleapis.com/v1/projects".toJavaUri
  case class Projects(
         projects: ?[Vector[Project]] = None,
    nextPageToken: ?[        String ] = None
  )

  import io.circe.generic.extras._
  implicit val config: Configuration = Configuration.default
  @ConfiguredJsonCodec
  case class Project(
    @JsonKey(         "name")   name:         String  ,
    @JsonKey(    "projectId")     id:         String  ,
    @JsonKey("projectNumber") number:         String  ,
                             buckets: ?[Paged[Bucket]]
  ) {
    def fetchBuckets(implicit config: Config): F[Project] =
      if (buckets.isEmpty) {
        implicit val project = this
        googleapis
          .storage
          .buckets()
          .map {
            buckets ⇒
              copy(
                buckets = Some(buckets)
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

  def apply()(implicit config: Config): F[Paged[Project]] = {
    println(s"Getting projects for auth ${config.auth}")
    Http(url)
      .json[Projects]
      .map {
        case Projects(items, nextPageToken) ⇒
          Paged(items.getOrElse(Vector()), nextPageToken)
      }
  }
}
