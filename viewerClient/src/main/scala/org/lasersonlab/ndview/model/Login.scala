package org.lasersonlab.ndview.model

import cats.implicits._
import org.lasersonlab.gcp.googleapis.User
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis
import org.lasersonlab.gcp.oauth.Auth
import org.lasersonlab.uri.{ http, _ }

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
