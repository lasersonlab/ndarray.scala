package org.lasersonlab.ndview.model

import cats.implicits._
import hammerlab.opt.dsl._
import org.lasersonlab.uri._
import org.lasersonlab.gcp
import org.lasersonlab.gcp.Config.implicits._
import org.lasersonlab.gcp.googleapis.Paged
import org.lasersonlab.gcp.googleapis.projects.Project

case class Projects(
  projects : Paged[Project],
  projectId: Option[String] = None
) {
  def apply(id: String): Project = projects.find(_.id == id).get
  def project: Option[Project] = projectId.map(apply)
  def select(id: String): Projects = copy(projectId = Some(id))

  def mod(id: String)(f: Δ[Project]): Projects = mod(Map(id → f))
  def mod(fns: (String, Δ[Project])*): Projects = mod(fns.toMap)
  def mod(fnMap: Map[String, Δ[Project]]): Projects =
    copy(
      projects =
        projects
          .copy(
            items =
              projects
                .items
                .foldLeft(Vector[Project]()) {
                  case (projects, next) ⇒
                    projects :+
                      fnMap
                        .get  { next.id }
                        .fold { next } { _(next) }
                }
          )
    )

  def +(projects: Paged[Project]): Projects = copy(this.projects + projects)

  def fetchBuckets(implicit cfg: gcp.Config): ?[F[Δ[Projects]]] =
    projects
      .flatMap {
        project ⇒
          project
            .fetchBuckets
            .map {
              _.map {
                project.id → _
              }
            }
      } match {
        case Vector() ⇒ None
        case projects ⇒
          Some(
            projects
              .sequence
              .map {
                fns ⇒
                  (projects: Projects) ⇒
                    projects.
                      mod(
                        fns
                          .map {
                            case (id, buckets) ⇒
                              id → { project: Project ⇒ project + buckets }
                          }
                          .toMap
                      )
              }
          )
      }
}

object Projects {
  implicit def unwrap(projects: Projects): Vector[Project] = projects.projects.items
  implicit def wrap(projects: Paged[Project]): Projects = Projects(projects)

  implicit def projectsΔ(Δ: (String, Δ[Project])): Δ[Projects] = _.mod(Δ)
}
