package org.lasersonlab.ndview.view

import java.lang.System.err

import cats.implicits._
import diode.react.ModelProxy
import io.circe.Printer
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomArray
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import lasersonlab.circe._
import org.lasersonlab.circe.SingletonCodec._
import org.lasersonlab.gcp.SignIn
import org.lasersonlab.gcp.SignIn.SignOut
import org.lasersonlab.gcp.oauth.scopes.auth._
import org.lasersonlab.gcp.oauth.{ Auth, ClientId, RedirectUrl, Scopes }
import org.lasersonlab.ndview.{ NewLogin, SelectProject, SelectUserProject, UpdateProjects }
import org.lasersonlab.ndview.model.{ Login, Logins }
import org.lasersonlab.uri.fragment
import org.scalajs.dom.document
import org.scalajs.dom.window.localStorage

import scala.concurrent.ExecutionContext

// Need this to take precedence over Encoder.encodeIterable; TODO: debug circe derivation that requires this
import org.lasersonlab.gcp.googleapis.Paged.pagedEncoder

object Page
extends SignIn.syntax
{
  type Props = (ModelProxy[Logins], ExecutionContext)

  implicit val CLIENT_ID = ClientId("218219996328-lltra1ss5e34hlraupaalrr6f56qmiat.apps.googleusercontent.com")
  implicit val REDIRECT_URL = RedirectUrl("http://localhost:8000")

  implicit val SCOPE =
    Scopes(
      userinfo email,
      userinfo profile,
      devstorage read_only,
      `cloud-platform` `read-only`
    )

  val stateKey = "app-state"

  val pprint = Printer.spaces4.copy(colonLeft = "").pretty _

  def initialState = {
    val str = localStorage.getItem(stateKey)
    Option(str)
      .fold {
        Logins()
      } {
        decode[Logins](_) match {
          case Left(e) ⇒
            err.println(s"Failed to parse state from localStorage:")
            err.println(e)
            err.println(str)
            localStorage.removeItem(stateKey)
            Logins()
          case Right(state) ⇒ state
        }
      }
  }

  def fetchBuckets(
    implicit
    model: ModelProxy[Logins],
    login: Login,
    ec: ExecutionContext
  ) = {
    println(s"fetching buckets for login $login on project change")
    implicit val auth = login.auth
    login
      .projects
      .fetchBuckets
      .fold { Callback() } {
        ΔF ⇒
          Callback.future {
            ΔF
              .map {
                Δ ⇒
                  println("got projects post-bucket-fetch")
                  model
                    .dispatchCB(
                      UpdateProjects(login.id, Δ)
                    )
              }
          }
      }
  }

  val component =
    ScalaComponent
      .builder[Props]("Page")
      .render {
        b ⇒
          import b._
          implicit val (model, ec) = props

          val state = model.value

          val logins = state.logins
          val login  = state.login

          println(s"render (${logins.size} logins; project ${login.flatMap(_.project).map(_.name)})")

          div(
            key := "page",
            className := "page",
            div(
              key := "controls",
              className := "controls"
            )(
              button(key := "sign-in", onClick --> { SignIn (); Callback() }, "sign in" ),
              login
                .map {
                  implicit login ⇒
                    implicit val auth = login.auth

                    VdomArray(
                      button(key := "sign-out", onClick --> { SignOut(); Callback() }, "sign out"),
                      ProjectSelect(login, id ⇒ fetchBuckets *> model.dispatchCB(    SelectProject(id)), login.    project,         "Project"),
                      ProjectSelect(login, id ⇒ fetchBuckets *> model.dispatchCB(SelectUserProject(id)), login.userProject, "Bill-to Project"),
                    )
                },
            ),

            login
              .fold { TagMod() } {
                login ⇒
                  implicit val Login(auth, user, projects, userProject) = login
                  for {
                    project ← login.project
                    buckets ← project.buckets
                  } yield
                    Buckets(
                      model,
                      login,
                      project,
                      buckets
                    )
              },

            div(
              key := "state",
              className := "state"
            )(
              Json(state.asJson)
            )
          )
      }
      .componentDidMount {
        p ⇒
          import p._
          println("did mount…")

          implicit val (model, ec) = props
          val state = model.value

          Auth
            .fromFragment(fragment.map)
            .fold(
              {
                _ ⇒
                  state
                    .login
                    .fold { Callback() } {
                      implicit login ⇒ fetchBuckets
                    }
              },
              {
                implicit auth ⇒
                  println(s"Processing fragment auth: $auth")
                  document.location.hash = ""
                  Callback.future(
                    Login()
                      .map {
                        login ⇒
                          println(s"got new login: $login")
                          model.dispatchCB(NewLogin(login))
                      }
                      .recover[CallbackTo[Unit]] {
                        case e ⇒
                          err.println("Failed to create login:")
                          err.println(e)
                          Callback()
                      }
                  )
              }
            )
      }
      .shouldComponentUpdate {
        p ⇒
          import p._
          CallbackTo(
            nextProps != currentProps ||
            nextState != currentState
          )
      }
      .componentWillUpdate {
        p ⇒
          localStorage
            .setItem(
              stateKey,
              pprint(
                p
                  .nextProps
                  ._1
                  .value
                  .asJson
              )
            )
          Callback()
      }
      .build

  def apply(logins: ModelProxy[Logins])(implicit ec: ExecutionContext) = component((logins, ec))
}
