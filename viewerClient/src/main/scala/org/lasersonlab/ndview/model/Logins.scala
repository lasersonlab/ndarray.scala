package org.lasersonlab.ndview.model

import cats.implicits._
import org.lasersonlab.gcp.googleapis.?
import org.lasersonlab.uri._

case class Logins(
  logins: Vector[Login] = Vector(),
  id: ?[String] = None
) {
  def login: Option[Login] = id.map(id ⇒ logins.find(_.id == id).get)

  def get(id: String) = logins.find(_.id == id)

  /**
   * Receive a new login, after as after an OAuth flow
   *
   * If a login exists for this user, just replace the [[org.lasersonlab.gcp.oauth.Auth]] credentials, otherwise add the
   * whole [[Login]]
   *
   * In either case, "select" its [[id]]
   */
  def :+(login: Login): Logins =
    Logins(
      logins
        .foldLeft(
          (
            false,
            Vector[Login]()
          )
        ) {
          case (
            (found, logins),
            next
          ) ⇒
            if (next.id == login.id) {
              println(s"Updating old login $next with new auth from $login")
              (
                true,
                logins :+ next.copy(auth = login.auth)
              )
            } else
              (
                found,
                logins :+ next
              )
        } match {
          case ( true, logins) ⇒ logins
          case (false, logins) ⇒ logins :+ login
        },
      Some(login.id)
    )

  def apply(Δ: Δ[Login]): Logins = apply(id.get)(Δ)
  def apply(id: String, select: Boolean = false)(Δ: Δ[Login]): Logins =
    copy(
      logins
        .foldLeft(Vector[Login]()) {
          (logins, next) ⇒
            logins :+ (
              if (next.id == id)
                Δ(next)
              else
                next
            )
        },
      id = if (select) Some(id) else this.id
    )
}

object Logins {
  implicit def loginsΔ(Δ: (String, Δ[Login])): Δ[Logins] = _.apply(Δ._1)(Δ._2)
  implicit def unwrap(logins: Logins): Vector[Login] = logins.logins
}
