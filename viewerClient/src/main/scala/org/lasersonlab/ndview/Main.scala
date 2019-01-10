package org.lasersonlab.ndview

import java.net.URLDecoder

import cats.effect.internals.IOContextShift
import cats.effect.{ Async, ExitCode, IO, IOApp }
import cats.implicits._
import org.lasersonlab.uri.gcp.{ Auth, GCS, Project, googleapis }
import org.scalajs.dom.document
import org.scalajs.dom.ext.AjaxException
import org.scalajs.dom.raw.{ Event, HTMLFormElement, HTMLInputElement }
import org.scalajs.dom.window.localStorage
import slinky.core._
import slinky.core.annotations.react
import slinky.web.ReactDOM
import slinky.web.html._

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.matching.Regex.Groups

object Main
  extends IOApp
{

  val CLIENT_ID = "1084141145491-i3sg6em7m7f8olghidnt7a0kc11n3dr8.apps.googleusercontent.com"
  val REDIRECT_URL = "http://localhost:8080/uri"
  val SCOPE = "https://www.googleapis.com/auth/devstorage.read_only"

  lazy val fragment = document.location.hash.drop(1)

  val fragmentRegex = """([^&=]+)=([^&]*)""".r

  val oauthEndpoint = "https://accounts.google.com/o/oauth2/v2/auth"
  def signIn(): Unit = {

    val form =
      document
        .createElement("form")
        .asInstanceOf[HTMLFormElement]

    form.method = "GET"
    form.action = oauthEndpoint

    val params =
      Map(
        "client_id" → CLIENT_ID,
        "redirect_uri" → REDIRECT_URL,
        "scope" → SCOPE,
        "include_granted_scopes" → "true",
        "response_type" → "token"
      )

    for {
      (name, value) ← params
    } {
      val input = document.createElement("input")
      input.setAttribute( "type", "hidden")
      input.setAttribute( "name",    name )
      input.setAttribute("value",   value )
      form.appendChild(input)
    }

    document.body.appendChild(form)
    localStorage.removeItem(credentialsKey)
    form.submit()
  }

  def fragmentMap: Map[String, String] =
    fragmentRegex
      .findAllMatchIn(fragment)
      .map {
        case Groups(k, v) ⇒
          URLDecoder.decode(k, "UTF-8") →
          URLDecoder.decode(v, "UTF-8")
      }
      .toMap

  implicit val ec = ExecutionContext.global

  val credentialsKey = "gcp-credentials"
  val stateKey = "app-state"

  implicit val ctx = IOContextShift.global

  import io.circe.generic.auto._
  import io.circe.parser.decode
  import io.circe.syntax._

//  @react class Buckets extends Component {
//    case
//  }

  @react class Page extends Component {
    type Props = Unit

    case class State(
      auth: Auth,
      project: Option[Project]
    ) {
      def projectDisplay = project.fold("")(_.toString)
    }

    def initialState = State(getAuth, None)

    implicit def auth: Auth = state.auth
    implicit def projectOpt: Option[Project] = state.project

    override def shouldComponentUpdate(nextProps: Props, nextState: State): Boolean =
      props != nextProps ||
      state != nextState

    override def componentWillUpdate(nextProps: Props, nextState: State) = {
      if (state.project != nextState.project) {

      }
    }

    def render = {
      println("render")

//      def readBinary() =
//        (GCS[IO]("ll-sc-data") / 'hca / "immune-cell-census" / "ica_cord_blood.10x.64m.zarr3" / 'GRCh38 / 'barcodes / 0)
//          .bytes(0, 2000)
//          .attempt
//          .map {
//            case Right(bytes) ⇒
//              println(s"test gcs req: ${bytes.take(100).mkString(" ")}")
//              ExitCode.Success
//            case Left(e) ⇒
//              println("gcs bytes err:")
//              println(e)
//              e.printStackTrace()
//              ExitCode.Error
//          }
//
//      def listDir() =
//        (GCS[IO]("ll-sc-data") / 'hca / "immune-cell-census" / "ica_cord_blood.10x.64m.zarr3" / 'GRCh38)
//          .list
//          .map {
//            files ⇒
//              files.foreach {
//                f ⇒
//                  println(s"found file: $f")
//              }
//              ExitCode.Success
//          }

      div(
        button(
          onClick := {
            _ ⇒ signIn()
          }
        )(
          "sign in"
        ),
        input(
          `type` := "text",
          placeholder := "project",
          onBlur := {
            e: Event ⇒
              val value =
                e
                  .target
                  .asInstanceOf[HTMLInputElement]
                  .value

              val project =
                if (value.nonEmpty)
                  Some(Project(value))
                else
                  None

              println(s"setting project: $project")
              setState(
                _.copy(
                  project = project
                )
              )
          }
        )
      )
    }

//    implicit def reqConfig =
//      org.lasersonlab.uri.http.Config(
//        headers = Map("Authorization" → s"Bearer ${auth.token}")
//      )

//    def buckets: IO[List[Metadata]] =
//      GCS[IO]("ll-sc-data", Vector("test.txt"))

    override def componentDidMount() = {
      println("did mount…")

      def readText() =
        GCS[IO]("ll-sc-data", Vector("test.txt"))
          .string
          .attempt
          .map {
            case Left(AjaxException(xhr)) if xhr.status == 401 ⇒
              println("caught 401, sign in again…")
              signIn()
              ???
            case e ⇒ e
          }
          .rethrow

      projectOpt.fold {
        println("no project set…")
      } {
        implicit project ⇒
          googleapis.storage.buckets[IO].map {
            case  Left(e) ⇒ println(s"buckets error: $e")
            case Right(e) ⇒ println(s"buckets success: $e")
          }
      }

//      println("running")
//      readText().unsafeRunAsync {
//        case Left(e) ⇒ println(s"run error: $e")
//        case Right(e) ⇒ println(s"run success: $e")
//      }
//      println("ran")
    }
  }

  def getAuth: Auth =
    Option(
      localStorage.getItem(credentialsKey)
    )
    .fold {
      Auth
        .fromFragment(fragmentMap)
        .map {
          auth ⇒
            val json = auth.asJson.noSpaces
            println(s"setting localstorage: $json")
            localStorage.setItem(credentialsKey, json)
            auth
        }
        .leftMap(new Exception(_))
    } {
      decode[Auth](_)
    }
    .fold(
      e ⇒ {
        println(s"no creds; sign in again")
        println(e)
        signIn()
        ???
      },
      r ⇒ r
      //_.copy(project = Some("hca-scale"))
    )

  override def run(args: List[String]): IO[ExitCode] = {

    println("client main")

    ReactDOM.render(
      Page(()),
      document.getElementById("root")
    )

    IO.pure(ExitCode.Success)
  }
}
