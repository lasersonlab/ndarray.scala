package org.lasersonlab.uri.gcp

import java.lang.System.err

import cats.ApplicativeError
import cats.implicits._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.lasersonlab.uri.fragment
import org.lasersonlab.uri.gcp.googleapis.Scope
import org.scalajs.dom.document
import org.scalajs.dom.ext.AjaxException
import org.scalajs.dom.raw.HTMLFormElement
import org.scalajs.dom.window.localStorage

object SignIn {
  case class Scopes(scopes: Scope*) {
    override def toString: String = scopes.mkString(" ")
  }
  case class    ClientId(override val toString: String)
  case class RedirectUrl(override val toString: String)

  implicit class Ops[F[_], T](val f: F[T]) extends AnyVal {
    def reauthenticate_?(
      implicit
      ae: ApplicativeError[F, Throwable],
      ClientId: ClientId,
      RedirectUrl: RedirectUrl,
      Scope: Scopes,
    ):
      F[T] =
      f
        .onError {
          case AjaxException(xhr) if xhr.status == 401 ⇒
            println("buckets req caught 401, sign in again…")
            SignIn().pure[F]
          case e ⇒
            err.println(e.getMessage)
            err.println(e)
            ().pure[F]
        }
  }

  val oauthEndpoint = "https://accounts.google.com/o/oauth2/v2/auth"
  val credentialsKey = "gcp-credentials"

  def apply()(
    implicit
    ClientId: ClientId,
    RedirectUrl: RedirectUrl,
    Scopes: Scopes,
  ): Unit = {

    val form =
      document
        .createElement("form")
        .asInstanceOf[HTMLFormElement]

    form.method = "GET"
    form.action = oauthEndpoint

    val params =
      Map(
        "client_id" → ClientId,
        "redirect_uri" → RedirectUrl,
        "scope" → Scopes,
        "include_granted_scopes" → "true",
        "response_type" → "token"
      )

    for {
      (name, value) ← params
    } {
      val input = document.createElement("input")
      input.setAttribute( "type", "hidden")
      input.setAttribute( "name",    name )
      input.setAttribute("value", value.toString )
      form.appendChild(input)
    }

    document.body.appendChild(form)
    localStorage.removeItem(credentialsKey)
    form.submit()
  }

  def SignOut(): Unit = { localStorage.removeItem(credentialsKey) }

  def loadAuth: Either[Exception, Auth] =
    Option(
      localStorage.getItem(credentialsKey)
    )
    .fold {
      Auth
        .fromFragment(fragment.map)
        .map {
          auth ⇒
            val json = auth.asJson.noSpaces
            println(s"setting localstorage: $json")
            localStorage.setItem(credentialsKey, json)
            auth
        }
        .leftMap(new Exception(_))  // TODO: use leftMap elsewhere
    } {
      str ⇒
        val auth = decode[Auth](str)
        document.location.hash = ""
        auth
    }

  trait syntax {
    @inline implicit def makeSignInOps[F[_], T](f: F[T]): Ops[F, T] = Ops(f)
  }
}
