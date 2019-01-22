package org.lasersonlab.ndview.view

import java.lang.System.err

import cats.implicits._
import hammerlab.bytes.Bytes
import io.circe.Printer
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.{ TagOf, VdomArray }
import org.lasersonlab.ndview.model.{ Login, Logins }
import org.lasersonlab.uri.fragment
import org.lasersonlab.uri.gcp.SignIn.{ ClientId, RedirectUrl, Scopes, SignOut }
import org.lasersonlab.uri.gcp.googleapis.Paged.pagedEncoder
import org.lasersonlab.uri.gcp.googleapis.scopes
import org.lasersonlab.uri.gcp.googleapis.storage.Bucket
import org.lasersonlab.uri.gcp.{ Auth, Metadata, SignIn, googleapis }
import org.scalajs.dom.document
import org.scalajs.dom.html.Div
import org.scalajs.dom.window.localStorage

import scala.concurrent.ExecutionContext

object Page
extends SignIn.syntax
{
  type Props = Unit
  type State = Logins

  implicit val CLIENT_ID = ClientId("218219996328-lltra1ss5e34hlraupaalrr6f56qmiat.apps.googleusercontent.com")
  implicit val REDIRECT_URL = RedirectUrl("http://localhost:8000")

  import scopes.auth._
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
        //println(s"state from localStorage: $str")
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
      .renderPS {
        (b, props, state) ⇒
          import b.setState
          implicit val ec = props

          def stateJson =
            div(
              key := "state",
              className := "state"
            )(
              Json(state.asJson)
            )

          println("render")
          val logins = state.logins
          val login  = state.login

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
                    VdomArray(
                      button(key := "sign-out", onClick --> { SignOut(); Callback() }, "sign out"),
                      ProjectSelect(login, id ⇒ setState { state.set(login.    project(id)) }, login.    project,         "Project"),
                      ProjectSelect(login, id ⇒ setState { state.set(login.userProject(id)) }, login.userProject, "Bill-to Project"),
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
                    div(
                      key := "buckets",
                      className := "buckets"
                    )(
                      buckets
                        .map {
                          case bucket @ Bucket(id, name, _, _, objects) ⇒
                            div(
                              key := id,
                              className := "bucket",
                              onClick -->
                                Callback.future {
                                  println("clicked")
                                  bucket
                                    .ls()
                                    .map {
                                      next ⇒
                                        println(s"Got new bucket: $next")
                                        setState {
                                          state
                                            .set {
                                              login
                                                .copy(
                                                  projects =
                                                    projects
                                                      .mod(project.id) {
                                                          project
                                                            .copy(
                                                              buckets =
                                                                Some(
                                                                  buckets
                                                                    .copy(
                                                                      items =
                                                                        buckets
                                                                          .items
                                                                          .map {
                                                                            case b if b.id == bucket.id ⇒ next
                                                                            case b ⇒ b
                                                                          }
                                                                    )
                                                                )
                                                            )
                                                      }
                                                )
                                            }
                                        }
                                    }
                                    .reauthenticate_?
                                }
                            )(
                              name,
                              objects
                                .map {
                                  objects ⇒
                                    (
                                      objects
                                        .dirs
                                        .map {
                                          dir ⇒
                                            div(
                                              key := dir,
                                              className := "dir"
                                            )(
                                              dir
                                            )
                                        } ++
                                      objects
                                        .files
                                        .map {
                                          case Metadata(_, name, size, _) ⇒
                                            div(
                                              key := name,
                                              className := "file"
                                            )(
                                              s"$name (${Bytes.format(size)})"
                                            )
                                        }
                                    )
                                    .toVdomArray
                                }
                            )
                        }: _*
                    )
              },

            stateJson
          )
      }
      .componentDidMount {
        p ⇒
          println("did mount…")
          Auth
            .fromFragment(fragment.map)
            .fold(
              {
                _ ⇒
                  p
                    .state
                    .login
                    .fold { Callback() } {
                      login ⇒
                        implicit val auth = login.auth
                        Callback.future {
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
                          p.modState(
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
          CallbackTo(
            p.nextProps != p.currentProps ||
            p.nextState != p.currentState
          )
      }
      .componentWillReceiveProps {
        p ⇒
          p.state
            .login
            .fold { Callback() } {
              login ⇒
                implicit val auth = login.auth
                Callback.future(
                  p.state
                    .modF {
                      case user ⇒
                        user
                          .projects
                          .fetchBuckets
                          .map {
                            projects ⇒
                              user.copy(
                                projects = projects
                              )
                          }
                    }
                    .map { p.setState }
                  )
            }
      }
      .componentWillUpdate {
        p ⇒
          localStorage.setItem(stateKey, pprint(p.nextState.asJson))
          Callback()
      }
      .build

  def apply()(implicit ec: ExecutionContext) = component(ec)
}
