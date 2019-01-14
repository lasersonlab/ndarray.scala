package org.lasersonlab.ndview

import lasersonlab.future.F
import org.lasersonlab.uri.gcp.{ Auth, googleapis }
import org.lasersonlab.uri.gcp.googleapis.Paged
import org.lasersonlab.uri.gcp.googleapis.projects.Project
import org.lasersonlab.uri.http

case class Projects(
  projects: Paged[Project],
  selected: Option[Project]
)

case class User(
  auth: Auth,
  user: googleapis.User,
  projects: Projects,
  userProject: Option[Project] = None
) {
  def project = projects.selected
}

object User {
  def apply(implicit auth: Auth, httpConfig: http.Config): F[User] =
    for {
      user ← googleapis.userinfo
      projects ← googleapis.projects
    } yield
      User(auth, user, projects)
}
