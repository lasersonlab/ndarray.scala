package org.lasersonlab.ndview

import java.net.{ URI, URLDecoder }

import cats.data.Nested
import cats.effect.internals.IOContextShift
import cats.effect.{ ConcurrentEffect, ExitCode, IO, IOApp }
import cats.implicits._
import com.softwaremill.sttp._
import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.lasersonlab.uri.Http
import org.lasersonlab.uri.gcp.{ Auth, GCS }
import org.scalajs.dom
import org.scalajs.dom.{ Event, document }
import org.scalajs.dom.ext.{ Ajax, AjaxException }
import org.scalajs.dom.raw.HTMLFormElement
import slinky.web.ReactDOM
import slinky.web.html._
import org.scalajs.dom.window.localStorage

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.matching.Regex.Groups
import scala.util.{ Failure, Success }

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

  case class Kind(override val toString: String)
  object Kind {
    def apply(c: HCursor)(implicit expected: Kind): Decoder.Result[Unit] =
      c
        .downField("kind")
        .as[String]
          .flatMap {
            case k if k == expected.toString ⇒ Right(k)
            case kind ⇒
              Left(
                DecodingFailure(
                  s"Expected kind '$expected', found '$kind'",
                  c.history
                )
              )
          }
  }

  case class Bucket(
    id: String,
    requesterPays: Boolean
  )
  object Bucket {
    implicit val kind = Kind("storage#bucket")
    implicit val decoder: Decoder[Bucket] =
      new Decoder[Bucket] {
        override def apply(c: HCursor): Result[Bucket] =
          for {
            _ ← Kind(c)
            id ←
              c
                .downField("id")
                .as[String]
            requesterPays =
              c
                .downField("billing")
                .downField("requesterPays")
                .as[Boolean]
                .getOrElse(false)
          } yield
            Bucket(
              id,
              requesterPays
            )
      }
  }
  case class Response[T](items: Seq[T])
  case class Buckets(value: Seq[Bucket])
  object Buckets {
    implicit val kind = Kind("storage#buckets")
    implicit val decoder: Decoder[Buckets] =
      new Decoder[Buckets] {
        override def apply(c: HCursor): Result[Buckets] =
          for {
            _ ← Kind(c)
            buckets ← c.downField("items").as[Seq[Bucket]]
          } yield
            Buckets(
              buckets
            )
      }
  }

  object googleapis {
    val base = uri"https://www.googleapis.com"
    object storage {
      val base = s"${googleapis.base}/storage/v1"
      object buckets {
        val base = s"${storage.base}/b"
        def list(
          project: String,
          userProject: Option[String] = None
        )(
          implicit
          auth: Auth
        ) = {
          Ajax(
            "HEAD",
            "https://www.googleapis.com/storage/v1/b/ll-sc-data/o/test.txt?userProject=hca-scale",
            data = null,
            timeout = 0,
            withCredentials = false,
            responseType = "",
            headers =
              Map(
                "Authorization" → s"Bearer ${auth.token}"
              ),

          )
//          Ajax.get(
//            uri"${buckets.base}?project=$project&userProject=$userProject".toString,
//            headers =
//              Map(
//                "Authorization" → s"Bearer ${auth.token}"
//              )
//          )
          .transformWith {
            case Failure(e) ⇒
              Future(
                e match {
                  case AjaxException(xhr) ⇒
                    xhr.status match {
                      case  401 ⇒
                        println("received 401; sign in again…")
                        Left(e)
                      case code ⇒ Left(e)
                    }
                  case _ ⇒ Left(e)
                }
              )
            case Success(r) ⇒
              println(s"response: ${r.responseText} ${r.response} ${r.getResponseHeader("Content-Length")}")
              Future(Right(Buckets(Nil)))
//              import io.circe.parser
//              Future(parser.decode[Buckets](r.responseText))
          }
        }
      }
    }
  }

  val credentialsKey = "gcp-credentials"

  implicit val ctx = IOContextShift.global

  override def run(args: List[String]): IO[ExitCode] = {

    println("client main")

    import io.circe.generic.auto._
    import io.circe.syntax._
    import io.circe.parser.decode

    ReactDOM.render(
      button(
        onClick := {
          _ ⇒ signIn()
        }
      )(
        "sign in"
      ),
      document.getElementById("button")
    )

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
        IO.pure(ExitCode.Success)
      },
      {
        auth: Auth ⇒
          implicit val _auth = auth.copy(project = Some("hca-scale"))

          implicit val reqConfig =
            org.lasersonlab.uri.http.Config(
              headers = Map("Authorization" → s"Bearer ${auth.token}")
            )

          println(s"doing http req…")

          GCS[IO]("ll-sc-data", Vector("test.txt"))
            .string
            .attempt
            .map {
              case Right(text) ⇒
                println(s"test gcs req: $text")
                ExitCode.Success
              case Left(e) ⇒
                println("gcs err")
                println(e)
                e.getCause
                e.printStackTrace()
                ExitCode.Error
            }
            .flatMap {
              exit ⇒
                GCS[IO]("ll-sc-data", Vector("hca", "immune-cell-census", "ica_cord_blood.10x.64m.zarr3", "GRCh38", "barcodes", "0"))
                  .bytes(0, 2000)
                  .attempt
                  .map {
                    case Right(bytes) ⇒
                      println(s"test gcs req: ${bytes.take(100).mkString(" ")}")
                      ExitCode.Success
                    case Left(e) ⇒
                      println("gcs err:")
                      println(e)
                      e.printStackTrace()
                      ExitCode.Error
                  }
            }

//          Nested(
//            googleapis
//              .storage
//              .buckets
//              .list(
////                "1084141145491"
//                "hca-scale",
//                Some("hca-scale")
//              )
//          )
//          .map {
//            buckets ⇒
//
//              ReactDOM.render(
//                div(
//                  for {
//                    Bucket(id, requesterPays) ← buckets.value
//                  } yield
//                    p(
//                      key := id
//                    )(
//                      s"$id $requesterPays"
//                    )
//                ),
//                document.getElementById("root")
//              )
//          }
      }
    )
  }
}
