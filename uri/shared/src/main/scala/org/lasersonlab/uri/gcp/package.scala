package org.lasersonlab.uri

package object gcp {
  implicit def reqConfig(implicit auth: Auth) =
    http.Defaults(
      headers = Map("Authorization" → s"Bearer ${auth.token}")
    )

  implicit def wrapProject(implicit project: Project): Option[Project] = Some(project)
}
