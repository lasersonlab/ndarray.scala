package org.lasersonlab.ndview

import cats.effect.{ ExitCode, IO, IOApp }
import cats.implicits._
import org.lasersonlab.uri.fragment
import org.lasersonlab.uri.gcp.googleapis.storage.{ Bucket, Objects }
import System.err

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._

import hammerlab.bytes._
import scala.util.{ Failure, Success }
import io.circe.Printer
import org.lasersonlab.uri._
import org.lasersonlab.uri.gcp.SignIn.{ ClientId, RedirectUrl, Scopes, SignOut }
import org.lasersonlab.uri.gcp.googleapis.{ Paged, scopes }
import org.lasersonlab.uri.gcp.googleapis.projects.Project
import org.lasersonlab.uri.gcp.{ Auth, Metadata, SignIn, googleapis }
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLSelectElement
import org.scalajs.dom.window.localStorage
import japgolly.scalajs.react._
import org.scalajs.dom.html
import scala.concurrent.ExecutionContext
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^._

// Need this to take precedence over Encoder.encodeIterable
import Paged.pagedEncoder

object Main
  extends IOApp
     with SignIn.syntax
{

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

  type Props = Unit
  type State = Logins

  val Page =
    ScalaComponent
      .builder[Unit]("Page")
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
      .render_PS {
        (props, state) ⇒
          div()
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
          import p._
          state
            .login
            .fold { Callback() } {
              login ⇒
                implicit val auth = login.auth
                Callback.future(
                  state
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
                    .map { setState }
                  )
            }
      }
      .componentWillUpdate {
        p ⇒
          localStorage.setItem(stateKey, pprint(p.nextState.asJson))
          Callback()
      }
      .build

//    def selectProject(
//      update: (Login, String) ⇒ Login,
//      project: Option[Project],
//      placeholder: String
//    ) =
//      state
//        .login
//        .fold[ReactElement]  {
//            select(disabled := true)
//        } {
//          case login @ Login(_, _, projects, _) ⇒
//            select(
//              value := project.fold[String] { "" } { _.id },
//              onChange := {
//                e ⇒
//                  val id =
//                    e
//                      .target
//                      .asInstanceOf[HTMLSelectElement]
//                      .value
//
//                  setState(
//                    _.set(
//                      update(login, id)
//                    )
//                  )
//              }
//            )(
//              option(
//                key := "_default",
//                value := "",
//                disabled := true,
//                  hidden := true
//              )(
//                s"$placeholder"
//              ) ::
//              projects
//                .map {
//                  case Project(name, id, _, _) ⇒
//                    option(
//                        key := id,
//                      value := id
//                    )(
//                      name
//                    )
//                }
//                .toList
//            )
//        }

//    def stateJson =
//      div(
//        className := "state"
//      )(
//        Json(state.asJson)
//      )
//
//    def render = {
//      println("render")
//      val logins = state.logins
//      val login  = state.login
//
//      login
//        .fold {
//          div(
//            button(onClick := { _ ⇒ SignIn () })("sign in" ),
//            stateJson
//          )
//        } {
//          login ⇒
//            implicit val Login(auth, user, projects, userProject) = login
//
//            div(
//              div(
//                className := "controls"
//              )(
//                button(onClick := { _ ⇒ SignIn () })("sign in" ),
//                button(onClick := { _ ⇒ SignOut() })("sign out"),
//
//                selectProject(_.    project(_), login.    project,         "Project"),
//                selectProject(_.userProject(_), login.userProject, "Bill-to Project"),
//              ),
//
//              for {
//                project ← login.project
//                buckets ← project.buckets
//              } yield
//                buckets
//                  .map {
//                    case bucket @ Bucket(id, name, _, _, objects) ⇒
//                      div(
//                        key := id,
//                        className := "bucket",
//                        onClick := {
//                          _ ⇒
//                            println("clicked")
//                            bucket
//                              .ls()
//                              .map {
//                                next ⇒
//                                  println(s"Got new bucket: $next")
//                                  setState {
//                                    _.set {
//                                      login
//                                        .copy(
//                                          projects =
//                                            projects
//                                              .mod(project.id) {
//                                                  project
//                                                    .copy(
//                                                      buckets =
//                                                        Some(
//                                                          buckets
//                                                            .copy(
//                                                              items =
//                                                                buckets
//                                                                  .items
//                                                                  .map {
//                                                                    case b if b.id == bucket.id ⇒ next
//                                                                    case b ⇒ b
//                                                                  }
//                                                            )
//                                                        )
//                                                    )
//                                              }
//                                        )
//                                    }
//                                  }
//                              }
//                              .reauthenticate_?
//                        }
//                      )(
//                        (s"$name": ReactElement) +:
//                        objects
//                          .fold {
//                            Vector[ReactElement]()
//                          } {
//                            objects ⇒
//                              objects
//                                .dirs
//                                .map {
//                                  dir ⇒
//                                    div(className := "dir")(dir)
//                                } ++
//                              objects
//                                .files
//                                .map {
//                                  case Metadata(_, name, size, _) ⇒
//                                    div(className := "file")(s"$name (${Bytes.format(size)})")
//                                }
//                          }: _*
//                      )
//                  }
//              ,
//              stateJson
//            )
//        }
//    }
//
//    override def componentDidMount() = {
//      println("did mount…")
//      Auth
//        .fromFragment(fragment.map)
//        .fold(
//          {
//            _ ⇒
//              state
//                .login
//                .foreach {
//                  login ⇒
//                    implicit val auth = login.auth
//                    login
//                      .projects
//                      .fetchBuckets
//                      .onComplete {
//                        case Success(projects) ⇒
//                          println("Updating projects after bucket-fetch attempt")
//                          setState ( state.map { _.copy(projects = projects) } )
//                        case Failure(e) ⇒
//                          err.println("Failed to fetch buckets:")
//                          err.println(e)
//                      }
//                }
//          },
//          {
//            implicit auth ⇒
//              println(s"Processing fragment auth: $auth")
//              document.location.hash = ""
//              Login()
//                .onComplete {
//                  case Success(login) ⇒
//                    println(s"got new login: $login")
//                    setState(
//                      state :+ login
//                    )
//                  case Failure(e) ⇒
//                    err.println("Failed to create login:")
//                    err.println(e)
//                }
//        }
//      )
//    }
//  }

  override def run(args: List[String]): IO[ExitCode] = {
    println("client main")
    Page().renderIntoDOM(document.getElementById("root"))
    IO.pure(ExitCode.Success)
  }
}
