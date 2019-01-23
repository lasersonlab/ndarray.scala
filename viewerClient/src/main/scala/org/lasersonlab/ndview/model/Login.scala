package org.lasersonlab.ndview.model

import cats.implicits._
import org.lasersonlab.gcp.googleapis.User
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis
import org.lasersonlab.gcp.oauth.Auth
import org.lasersonlab.uri.{ Δ, _ }

case class Login(
  auth: Auth,
  user: User,
  projects: Projects,
  userProject: ?[Project] = None
) {
  def id = user.id
  def project = projects.project
  def     project(id: String): Login = copy(projects = projects.select(id))
  def userProject(id: String): Login = copy(userProject = projects(id))
  def +(projects: Projects) = copy(projects = this.projects + projects.projects)

  def apply(Δ: Δ[Projects]): Login = copy(projects = Δ(projects))

  import Projects._
  def apply(id: String)(Δ: Δ[Project]): Login = apply((id, Δ))
}

object Login {
  def apply()(implicit auth: Auth, httpConfig: http.Config): F[Login] =
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
