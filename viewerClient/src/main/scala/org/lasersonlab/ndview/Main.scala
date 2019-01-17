package org.lasersonlab.ndview

import cats.effect.{ ExitCode, IO, IOApp }
import cats.implicits._
import org.lasersonlab.uri.gcp.googleapis.storage.Bucket
//import org.lasersonlab.uri.gcp.googleapis.?
import io.circe.Printer
import org.lasersonlab.uri.gcp.SignIn.{ ClientId, RedirectUrl, Scope, SignOut, loadAuth }
import org.lasersonlab.uri.gcp.googleapis.projects.Project
import org.lasersonlab.uri.gcp.{ Auth, SignIn, googleapis }
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLSelectElement
import org.scalajs.dom.window.localStorage
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.ReactDOM
import slinky.web.html._

import scala.concurrent.ExecutionContext

object Main
  extends IOApp
     with SignIn.syntax
{

  implicit val CLIENT_ID = ClientId("218219996328-lltra1ss5e34hlraupaalrr6f56qmiat.apps.googleusercontent.com")
  implicit val REDIRECT_URL = RedirectUrl("http://localhost:8000")
  implicit val SCOPE =
    Scope(
      "https://www.googleapis.com/auth/userinfo.profile",
      "https://www.googleapis.com/auth/devstorage.read_only",
      "https://www.googleapis.com/auth/cloud-platform.read-only"
    )

  implicit val ec = ExecutionContext.global

  val stateKey = "app-state"

  import System.err

  import io.circe.generic.auto._
  import io.circe.parser.decode
  import io.circe.syntax._

  val pprint = Printer.spaces4.copy(colonLeft = "").pretty _

  @react class Page extends Component {
    type Props = Unit

    type State = Logins
//    case class State(
//          project : Option[     Project ] = None,
//      userProject : Option[ UserProject ] = None,
//          projects: Option[List[Project]] = None,
//    )

    def initialState: State = {
      val str = localStorage.getItem(stateKey)
      println(s"state from localStorage: $str")
      Option(str)
        .fold {
          Logins()
        } {
          decode[State](_) match {
            case Left(e) ⇒
              err.println(s"Failed to parse state from localStorage: $str; clearing")
              localStorage.removeItem(stateKey)
              initialState
            case Right(state) ⇒ state
          }
        }
    }

    //implicit def auth: Auth = props

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
                      login.project(id)
                    )
                  )
              }
            )(
              option(
                key := "_default",
                value := "",
                selected := true,
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

    def render = {
      println("render")

      val logins = state.logins
      val login  = state.login

      login
        .fold {
          div(
            button(onClick := { _ ⇒ SignIn () })("sign in" ),
          )
        } {
          login ⇒
            implicit val Login(auth, user, projects, userProject) = login

            div(
              button(onClick := { _ ⇒ SignIn () })("sign in" ),
              button(onClick := { _ ⇒ SignOut() })("sign out"),

              selectProject(login.    project,         "Project"),
              selectProject(login.userProject, "Bill-to Project"),

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
            )
        }
    }

//    def buckets(
//      implicit
//      auth: Auth,
//      project: Project,
//      userProject: ?[UserProject]
//    ): F[Unit] = {
//      println(s"got project: $project")
//      googleapis
//        .storage
//        .buckets
//        .map {
//          buckets ⇒
//            println(s"got ${buckets.size} buckets: ${buckets.mkString(",")}")
//            val newProject = project.copy(buckets = buckets)
//            setState(
//              _.copy(
//                project = newProject,
//                projects =
//                  state
//                    .projects
//                    .map {
//                      _.map {
//                        case p
//                          if p.id == project.id ⇒
//                          newProject
//                        case p ⇒ p
//                      }
//                    }
//              )
//            )
//        }
//        .reauthenticate_?
//    }

    override def componentDidMount() = {
      println("did mount…")
      state
        .login
        .foreach {
          login ⇒
            implicit val auth = login.auth
            println("fetching projects…")
            googleapis
              .projects()
              .map {
                projects ⇒
                  println(s"got ${projects.size} projects: ${projects.map(_.name).mkString(",")}")
                  setState(
                    state
                      .map {
                        _.copy(
                          projects = projects
                        )
                      }
                  )
              }
              .reauthenticate_?
        }
    }

  }

  override def run(args: List[String]): IO[ExitCode] = {
    println("client main")
    loadAuth match {
      case Left(e) ⇒
        println(s"no creds; sign in again")
        println(e)
        SignIn()
        loadAuth
      case Right(auth) ⇒
        ReactDOM.render(
          Page(auth),
          document.getElementById("root")
        )
    }

    IO.pure(ExitCode.Success)
  }
}
