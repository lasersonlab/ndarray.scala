package org.lasersonlab.gcp

import java.lang.System.err

import cats.ApplicativeError
import cats.implicits._
import io.circe.generic.auto._
import org.lasersonlab.gcp.oauth.Params
import org.scalajs.dom.document
import org.scalajs.dom.ext.AjaxException
import org.scalajs.dom.raw.HTMLFormElement
import org.scalajs.dom.window.localStorage

object SignIn {

  implicit class Ops[F[_], T](val f: F[T]) extends AnyVal {
    def reauthenticate_?(
      implicit
      ae: ApplicativeError[F, Throwable],
      params: Params
    ):
      F[T] =
      f
        .onError {
          case AjaxException(xhr) if xhr.status == 401 ⇒
            err.println("buckets req caught 401, sign in again…")
            SignIn().pure[F]
          case e ⇒
            err.println(e.getMessage)
            err.println(e)
            ().pure[F]
        }
  }

  val oauthEndpoint = "https://accounts.google.com/o/oauth2/v2/auth"

  def apply()(
    implicit
    params: Params
  ): Unit = {

    val form =
      document
        .createElement("form")
        .asInstanceOf[HTMLFormElement]

    form.method = "GET"
    form.action = oauthEndpoint

    for {
      (name, value) ← params.map
    } {
      val input = document.createElement("input")
      input.setAttribute( "type", "hidden")
      input.setAttribute( "name",    name )
      input.setAttribute("value", value.toString )
      form.appendChild(input)
    }

    document.body.appendChild(form)
    form.submit()
  }

  trait syntax {
    @inline implicit def makeSignInOps[F[_], T](f: F[T]): Ops[F, T] = Ops(f)
  }
  object syntax extends syntax
}
