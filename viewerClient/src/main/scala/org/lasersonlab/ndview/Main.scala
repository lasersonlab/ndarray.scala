package org.lasersonlab.ndview

import cats.effect.{ ExitCode, IO, IOApp }
import cats.implicits._
import hammerlab.option.liftOption
import io.circe.Printer
import lasersonlab.future.F
import org.lasersonlab.uri.gcp.SignIn.{ ClientId, RedirectUrl, Scope, SignOut, loadAuth }
import org.lasersonlab.uri.gcp.googleapis.projects.{ Project, UserProject }
import org.lasersonlab.uri.gcp.{ Auth, GCS, SignIn, googleapis }
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
    type Props = Auth

    case class State(
          project : Option[     Project]  = None,
      userProject : Option[ UserProject]  = None,
          projects: Option[List[Project]] = None,
    )

    def initialState: State = {
      val str = localStorage.getItem(stateKey)
      println(s"state from localStorage: $str")
      Option(str)
        .fold {
          State()
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

    implicit def auth: Auth = props

    override def shouldComponentUpdate(nextProps: Props, nextState: State): Boolean =
      props != nextProps ||
      state != nextState

    override def componentWillUpdate(nextProps: Props, nextState: State) = {
      val nextProject = nextState.project
      println(s"componentWillUpdate? ${state.project} $nextProject")
      nextProject match {
        case Some(p @ Project(_, _, _, None)) ⇒
          buckets(p, nextState.userProject)
        case _ ⇒
      }
      localStorage.setItem(stateKey, pprint(nextState.asJson))
    }

    def selectProject(
      setProject: (State, Project) ⇒ State,
      projection: State ⇒ Option[Project],
      placeholder: String
    ) =
      state
        .projects
        .fold[ReactElement] {
          select(disabled := true)
        } {
          projects ⇒
            val project = projection(state)
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
                    _.copy(
                      project =
                        projects
                          .find(_.id == id)
                          .get
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
                  case p @ Project(name, id, number, _) ⇒
                    option(
                        key := id,
                      value := id
                    )(
                      name
                    )
                }
            )
        }

    def render = {
      println("render")

      val State(project, userProject, projects) = state

      div(
        button(onClick := { _ ⇒ SignIn () })("sign in" ),
        button(onClick := { _ ⇒ SignOut() })("sign out"),

        selectProject((state, project) ⇒ state.copy(    project =             project ), _                  .project , placeholder = "Project"),
        selectProject((state, project) ⇒ state.copy(userProject = UserProject(project)), _.userProject.map(_.project), placeholder = "'Bill-to' Project"),

        for {
          project ← project
          buckets ← project.buckets
        } yield
          buckets
            .buckets
            .map {
              bucket ⇒
                div(
                  key := bucket.id
                )(
                  s"$bucket"
                )
            }
      )
    }

    def buckets(implicit project: Project, userProject: Option[UserProject]): F[Unit] = {
      println(s"got project: $project")
      googleapis
        .storage
        .buckets
        .map {
          buckets ⇒
            println(s"got ${buckets.size} buckets: ${buckets.mkString(",")}")
            val newProject = project.copy(buckets = buckets)
            setState(
              _.copy(
                project = newProject,
                projects =
                  state
                    .projects
                    .map {
                      _.map {
                        case p
                          if p.id == project.id ⇒
                          newProject
                        case p ⇒ p
                      }
                    }
              )
            )
        }
        .reauthenticate_?
    }

    override def componentDidMount() = {
      println("did mount…")

      if (state.projects.isEmpty) {
        println("fetching projects…")
        googleapis
          .projects
          .apply
          .map {
            projects ⇒
              println(s"got ${projects.size} projects: ${projects.map(_.name).mkString(",")}")
              setState(_.copy(projects = projects))
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
