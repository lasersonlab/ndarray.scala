package org.lasersonlab.ndview

import cats.implicits._
import org.lasersonlab.uri._
import org.lasersonlab.uri.gcp.Config.implicits._
import org.lasersonlab.uri.gcp.googleapis.Paged
import org.lasersonlab.uri.gcp.googleapis.projects.Project

import scala.concurrent.ExecutionContext

case class Projects(
  projects : Paged[Project],
  projectId: Option[String] = None
) {
  def apply(id: String): Project = projects.find(_.id == id).get
  def project: Option[Project] = projectId.map(apply)
  def select(id: String): Projects = copy(projectId = Some(id))
  def mod(id: String)(project: Project): Projects =
    copy(
      projects =
        projects
          .copy(
            items =
              projects
                .items
                .foldLeft(Vector[Project]()) {
                  case (builder, cur) ⇒
                    builder :+ (if (cur.id == id) project else cur)
                }
          )
    )

  def modF(pf: PartialFunction[Project, F[Project]])(implicit ec: ExecutionContext): F[Projects] = {
    def f(project: Project) = if (pf.isDefinedAt(project)) pf(project) else F { project }
    projects
      .items
      .traverse { f }
      .map {
        projects ⇒
          Projects(
            this
              .projects
              .copy(
                items = projects
              ),
            projectId
          )
      }
  }
  def fetchBuckets(implicit cfg: gcp.Config): F[Projects] =
    modF {
      case project @ Project(_, _, _, None) ⇒
        project.fetchBuckets
    }
}

object Projects {
  implicit def unwrap(projects: Projects): Vector[Project] = projects.projects.items
  implicit def wrap(projects: Paged[Project]): Projects = Projects(projects)
}
