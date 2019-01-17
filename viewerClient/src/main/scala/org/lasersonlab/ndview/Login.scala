package org.lasersonlab.ndview

import cats.implicits._
import hammerlab.option.Non
import lasersonlab.future.F
import org.lasersonlab.uri.gcp.{ Auth, googleapis }
import org.lasersonlab.uri.gcp.googleapis.{ Paged, User }
import org.lasersonlab.uri.gcp.googleapis.projects.Project
import org.lasersonlab.uri.http

import scala.concurrent.ExecutionContext

case class Projects(
  projects : Paged[Project],
  projectId: Option[String] = None
) {
  def project: Option[Project] = projectId.map(id ⇒ projects.items.find(_.id == id).get)
  def select(id: String): Projects = copy(projectId = Some(id))
  def mod(pf: PartialFunction[Project, F[Project]])(implicit ec: ExecutionContext): F[Projects] = {
    def f(project: Project) = if (pf.isDefinedAt(project)) pf(project) else F { project }
    projects
      .items
      .map { f }
      .sequence
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
  def fetchBuckets(implicit auth: Auth, h: http.Config): F[Projects] =
    mod {
      case project @ Project(_, _, _, Non) ⇒
        project.fetchBuckets
    }
}
object Projects {
  implicit def unwrap(projects: Projects): Vector[Project] = projects.projects.items
  implicit def wrap(projects: Paged[Project]): Projects = Projects(projects)
}

case class Logins(
  logins: List[Login] = Nil,
  id: Option[String] = None
) {
  def login: Option[Login] = id.map(id ⇒ logins.find(_.id == id).get)
  def set(newLogin: Login): Logins = map { _ ⇒ newLogin }
  def map(f: Login ⇒ Login): Logins = mod { case login if id.contains(login.id) ⇒ f(login) }
  def modF(pf: PartialFunction[Login, F[Login]])(implicit ec: ExecutionContext): F[Logins] = {
    def f(login: Login) = if (pf.isDefinedAt(login)) pf(login) else F { login }
    logins
      .map(f)
      .sequence
      .map {
        Logins(
          _,
          id
        )
      }
  }
  def mod(pf: PartialFunction[Login, Login]): Logins = {
    def f(login: Login) = if (pf.isDefinedAt(login)) pf(login) else login
    Logins(
      logins.map(f),
      id
    )
  }
}

case class Login(
  auth: Auth,
  user: User,
  projects: Projects,
  userProject: Option[Project] = None
) {
  def id = user.id
  def project = projects.project
  def project(id: String): Login = copy(projects = projects.select(id))
}

object Login {
  def apply(implicit auth: Auth, httpConfig: http.Config): F[Login] =
    for {
      user ← googleapis.userinfo()
      projects ← googleapis.projects()
    } yield
      Login(
        auth,
        user,
        projects
      )
}
