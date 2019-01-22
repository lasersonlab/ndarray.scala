package org.lasersonlab.ndview.view

import java.lang.System.err

import cats.implicits._
import io.circe.Printer
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomArray
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import org.lasersonlab.circe.SingletonCodec._
import org.lasersonlab.gcp.SignIn
import org.lasersonlab.gcp.SignIn.SignOut
import org.lasersonlab.gcp.oauth.scopes.auth._
import org.lasersonlab.gcp.oauth.{ Auth, ClientId, RedirectUrl, Scopes }
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
  type Props = Unit
  type State = Logins

  implicit val CLIENT_ID = ClientId("218219996328-lltra1ss5e34hlraupaalrr6f56qmiat.apps.googleusercontent.com")
  implicit val REDIRECT_URL = RedirectUrl("http://localhost:8000")

  implicit val SCOPE =
    Scopes(
      userinfo email,
      userinfo profile,
      devstorage read_only,
      `cloud-platform` `read-only`
    )

  implicit val ec = ExecutionContext.global

  val stateKey = "app-state"

  val pprint = Printer.spaces4.copy(colonLeft = "").pretty _

  val component =
    ScalaComponent
      .builder[ExecutionContext]("Page")
      .initialState {
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
      .render {
        b ⇒
          import b._
          implicit val ec = props

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
                  login ⇒
                    implicit val auth = login.auth
                    def fetchBuckets =
                      Callback.future {
                        println(s"fetching buckets for login $login on project change")
                        login
                          .projects
                          .fetchBuckets
                          .map {
                            projects ⇒
                              println("got projects post-bucket-fetch")
                              modState {
                                state ⇒
                                  state.mod(login.id) {
                                    login ⇒
                                      login
                                        .copy(
                                          projects =
                                            login
                                              .projects
                                              .copy(
                                                projects = projects.projects
                                              )
                                        )
                                  }
                              }
                          }
                      }

                    VdomArray(
                      button(key := "sign-out", onClick --> { SignOut(); Callback() }, "sign out"),
                      ProjectSelect(login, id ⇒ fetchBuckets *> modState { s ⇒ println(s"setting project to $id"); s.set(login.    project(id)) }, login.    project,         "Project"),
                      ProjectSelect(login, id ⇒ fetchBuckets *> modState { _.set(login.userProject(id)) }, login.userProject, "Bill-to Project"),
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
                      login,
                      project,
                      buckets,
                      f ⇒ modState { _.mod(login.id) { f } }
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
          Auth
            .fromFragment(fragment.map)
            .fold(
              {
                _ ⇒
                  state
                    .login
                    .fold { Callback() } {
                      login ⇒
                        implicit val auth = login.auth
                        Callback.future {
                          println(s"componentDidMount: fetching buckets for login $login")
                          login
                            .projects
                            .fetchBuckets
                            .map {
                              projects ⇒
                                println("Updating projects after bucket-fetch attempt")
                                p.modState ( _.map { _.copy(projects = projects) } )
                            }
                            .recover[CallbackTo[Unit]] {
                              case e ⇒
                                err.println("Failed to fetch buckets:")
                                err.println(e)
                                Callback()
                            }
                        }
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
                          modState(
                            _ :+ login
                          )
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
          localStorage.setItem(stateKey, pprint(p.nextState.asJson))
          Callback()
      }
      .build

  def apply()(implicit ec: ExecutionContext) = component(ec)
}
