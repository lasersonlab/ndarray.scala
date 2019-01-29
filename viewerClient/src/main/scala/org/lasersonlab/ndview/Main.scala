package org.lasersonlab.ndview

import cats.implicits._
import io.circe.generic.auto._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.Implicits._
import lasersonlab.circe._
import lasersonlab.future._
import org.lasersonlab.gcp.SignIn
import org.lasersonlab.gcp.oauth._
import org.lasersonlab.gcp.oauth.scopes.auth._
import org.lasersonlab.ndview.model.Login
import org.lasersonlab.ndview.view.{ LocalStorage, Page }
import org.lasersonlab.uri.fragment
import org.scalajs.dom.{ document, window }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.JSApp

object Main
  extends JSApp
     with SignIn.syntax
{
  implicit val CLIENT_ID = ClientId("218219996328-lltra1ss5e34hlraupaalrr6f56qmiat.apps.googleusercontent.com")
  implicit val REDIRECT_URL = RedirectUrl("http://localhost:8000")

  implicit val SCOPE =
    Scopes(
       userinfo email,
       userinfo profile,
       devstorage read_only,
      `cloud-platform``read-only`
    )

  def main() = {
    println("client main")

    val model = LocalStorage(Model())

    // If this is an OAuth redirect:
    //
    // - pull new credentials from fragment
    // - clear new credentials from fragment
    // - rely on persisted LocalStorage state to tell us what path to restore
    Auth
      .fromFragment(fragment.map)
      .toOption
      .map {
        implicit auth ⇒
          println(s"Found fragment auth: $auth")
          document.location.hash = ""
          Login().logError
      }
      .fold { F { model } } {
        _.map {
          login ⇒
            model.copy(
              logins = model.logins :+ login
            )
        }
      }
      .map {
        model ⇒

        val circuit = Circuit(model)
        val connection = circuit.connect(_.logins)

        val base = BaseUrl(window.location.href.takeWhile(_ != '#'))

        val routerConfig = RouterConfigDsl[Vector[String]].buildConfig { dsl ⇒
          import dsl._

          (
            dynamicRoute(
              "#/" ~
              remainingPathOrBlank
                .xmap[Vector[String]] {
                  _.split("/") match {
                    case Array("") ⇒ Vector()
                    case arr ⇒ arr.toVector
                  }
                } {
                  _.mkString("/")
                }
            ) {
              case p ⇒ p
            } ~> dynRenderR {
              (path, ctl) ⇒
                implicit val _ctl = ctl
                connection {
                  implicit proxy ⇒
                    Page(Model(proxy.value, path))
                }
            }
          )
          .notFound(redirectToPage(Vector())(Redirect.Replace))
        }

        val router = Router(base, routerConfig.logToConsole)

        router()
          .renderIntoDOM(
            document.getElementById("root")
          )
      }
  }
}
