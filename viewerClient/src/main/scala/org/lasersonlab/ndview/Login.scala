package org.lasersonlab.ndview

import cats.implicits._
import org.lasersonlab.uri._
import org.lasersonlab.uri.gcp.{ Auth, googleapis }
import org.lasersonlab.uri.gcp.googleapis.{ Paged, User }
import org.lasersonlab.uri.gcp.googleapis.projects.Project
import org.lasersonlab.uri.http

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
