package org.lasersonlab.ndview.model

import cats.implicits._
import org.lasersonlab.uri.F
import org.lasersonlab.uri.gcp.googleapis.?

import scala.concurrent.ExecutionContext

case class Logins(
  logins: Vector[Login] = Vector(),
  id: ?[String] = None
) {
  def login: Option[Login] = id.map(id ⇒ logins.find(_.id == id).get)
  def :+(login: Login): Logins =
    copy(
      logins
        .partition {
          _.id == login.id
        } match {
          case (Vector(old), rest) ⇒
            println(s"Updating old login $old with new auth from $login")
            rest :+ old.copy(auth = login.auth)
          case (Vector(), rest) ⇒
            println(s"Didn't find new login $login in existing logins ${logins.map(_.id).mkString(",")}")
            rest :+ login
        }
    )
  def set(newLogin: Login): Logins = map { _ ⇒ newLogin }
  def map(f: Login ⇒ Login): Logins = mod { case login if id.contains(login.id) ⇒ f(login) }
  def modF(pf: PartialFunction[Login, F[Login]])(implicit ec: ExecutionContext): F[Logins] = {
    def f(login: Login) = if (pf.isDefinedAt(login)) pf(login) else F { login }
    logins
      .traverse(f)
      .map { copy(_) }
  }
  def mod(pf: PartialFunction[Login, Login]): Logins = {
    def f(login: Login) = if (pf.isDefinedAt(login)) pf(login) else login
    copy(
      logins.map(f)
    )
  }
}
