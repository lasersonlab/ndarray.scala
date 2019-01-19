package org.lasersonlab.ndview

import cats.effect.{ ExitCode, IO, IOApp }
import cats.implicits._
import org.lasersonlab.uri.fragment
import org.lasersonlab.uri.gcp.googleapis.storage.Bucket

import System.err

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._

import scala.util.{ Failure, Success }
import io.circe.Printer
import org.lasersonlab.uri._
import org.lasersonlab.uri.gcp.SignIn.{ ClientId, RedirectUrl, Scope, SignOut }
import org.lasersonlab.uri.gcp.googleapis.Paged
import org.lasersonlab.uri.gcp.googleapis.projects.Project
import org.lasersonlab.uri.gcp.{ Auth, SignIn, googleapis }
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLSelectElement
import org.scalajs.dom.window.localStorage
import scalajs.js.Dynamic.literal
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.ReactDOM
import slinky.web.html._

import scala.concurrent.ExecutionContext

// Need this to take precedence over Encoder.encodeIterable
import Paged.pagedEncoder

object Main
  extends IOApp
     with SignIn.syntax
{

  implicit val CLIENT_ID = ClientId("218219996328-lltra1ss5e34hlraupaalrr6f56qmiat.apps.googleusercontent.com")
  implicit val REDIRECT_URL = RedirectUrl("http://localhost:8000")
  implicit val SCOPE =
    Scope(
      "https://www.googleapis.com/auth/userinfo.email",
      "https://www.googleapis.com/auth/userinfo.profile",
      "https://www.googleapis.com/auth/devstorage.read_only",
      "https://www.googleapis.com/auth/cloud-platform.read-only"
    )

  implicit val ec = ExecutionContext.global

  val stateKey = "app-state"

  val pprint = Printer.spaces4.copy(colonLeft = "").pretty _

  @react class Page extends Component {
    type Props = Unit
    type State = Logins

    def initialState: State = {
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

    override def shouldComponentUpdate(nextProps: Props, nextState: State): Boolean =
      props != nextProps ||
      state != nextState

    override def componentWillUpdate(nextProps: Props, nextState: State) = {
      nextState
        .login
        .foreach {
          login ⇒
            implicit val auth = login.auth
            nextState
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
              .foreach { setState }
        }

      localStorage.setItem(stateKey, pprint(nextState.asJson))
    }

    def selectProject(
      update: (Login, String) ⇒ Login,
      project: Option[Project],
      placeholder: String
    ) =
      state
        .login
        .fold[ReactElement]  {
            select(disabled := true)
        } {
          case login @ Login(_, _, projects, _) ⇒
            select(
              value := project.fold[String] { "" } { _.id },
              onChange := {
                e ⇒
                  val id =
                    e
                      .target
                      .asInstanceOf[HTMLSelectElement]
                      .value

                  setState(
                    _.set(
                      update(login, id)
                    )
                  )
              }
            )(
              option(
                key := "_default",
                value := "",
                disabled := true,
                  hidden := true
              )(
                s"$placeholder"
              ) ::
              projects
                .map {
                  case Project(name, id, _, _) ⇒
                    option(
                        key := id,
                      value := id
                    )(
                      name
                    )
                }
                .toList
            )
        }

    def stateJson =
      div(
        style := literal(
          marginTop = "2em",
          fontSize = "0.8em",
          fontFamily = "monospace"
        )
      )(
        Json(state.asJson)
      )

    def render = {
      println("render")
      val logins = state.logins
      val login  = state.login

      login
        .fold {
          div(
            button(onClick := { _ ⇒ SignIn () })("sign in" ),
            stateJson
          )
        } {
          login ⇒
            implicit val Login(auth, user, projects, userProject) = login

            div(
              div(
                className := "controls"
              )(
                button(onClick := { _ ⇒ SignIn () })("sign in" ),
                button(onClick := { _ ⇒ SignOut() })("sign out"),

                selectProject(_.    project(_), login.    project,         "Project"),
                selectProject(_.userProject(_), login.userProject, "Bill-to Project"),
              ),

              for {
                project ← login.project
                buckets ← project.buckets
              } yield
                buckets
                  .map {
                    case Bucket(id, name, _, _) ⇒
                      div(
                        key := id
                      )(
                        s"$name"
                      )
                  }
              ,
              stateJson
            )
        }
    }

    override def componentDidMount() = {
      println("did mount…")
      Auth
        .fromFragment(fragment.map)
        .fold(
          {
            _ ⇒
              state
                .login
                .foreach {
                  login ⇒
                    implicit val auth = login.auth
                    login
                      .projects
                      .fetchBuckets
                      .onComplete {
                        case Success(projects) ⇒
                          println("Updating projects after bucket-fetch attempt")
                          setState ( state.map { _.copy(projects = projects) } )
                        case Failure(e) ⇒
                          err.println("Failed to fetch buckets:")
                          err.println(e)
                      }
                }
          },
          {
            implicit auth ⇒
              println(s"Processing fragment auth: $auth")
              document.location.hash = ""
              Login()
                .onComplete {
                  case Success(login) ⇒
                    println(s"got new login: $login")
                    setState(
                      state :+ login
                    )
                  case Failure(e) ⇒
                    err.println("Failed to create login:")
                    err.println(e)
                }
        }
      )
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    println("client main")
    ReactDOM.render(
      Page(),
      document.getElementById("root")
    )

    IO.pure(ExitCode.Success)
  }
}
