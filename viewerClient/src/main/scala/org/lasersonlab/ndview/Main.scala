package org.lasersonlab.ndview

import java.net.URLDecoder

import cats.data.Nested
import cats.implicits._
import com.softwaremill.sttp._
import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.scalajs.dom.document
import org.scalajs.dom.ext.{ Ajax, AjaxException }
import org.scalajs.dom.raw.HTMLFormElement
import slinky.web.ReactDOM
import slinky.web.html._

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.matching.Regex.Groups
import scala.util.{ Failure, Success }

object Main {

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
    form.submit()
  }

  def decode(s: String): String = URLDecoder.decode(s, "UTF-8")

  def fragmentMap: Map[String, String] =
    fragmentRegex
      .findAllMatchIn(fragment)
      .map {
        case Groups(k, v) ⇒
          decode(k) →
          decode(v)
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
          Ajax.get(
            uri"${buckets.base}?project=$project&userProject=$userProject".toString,
            headers =
              Map(
                "Authorization" → s"Bearer ${auth.token}"
              )
          )
          .transformWith {
            case Failure(e) ⇒
              Future(
                e match {
                  case AjaxException(xhr) ⇒
                    xhr.status match {
                      case  401 ⇒ signIn(); ???
                      case code ⇒ Left(e)
                    }
                  case _ ⇒ Left(e)
                }
              )
            case Success(r) ⇒
              import io.circe.parser
              Future(parser.decode[Buckets](r.responseText))
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val map = fragmentMap

    println("client main")

    Auth
      .fromFragment(map)
      .fold(
        _ ⇒ signIn(),
        {
          implicit auth: Auth ⇒

            Nested[Future, Either[Throwable, ?], Buckets](
              googleapis
                .storage
                .buckets
                .list(
                  "hca-scale",
                  Some("hca-scale")
                )
            )
            .map {
              buckets ⇒

                ReactDOM.render(
                  div(
                    for {
                      Bucket(id, requesterPays) ← buckets.value
                    } yield
                      p(
                        key := id
                      )(
                        s"$id $requesterPays"
                      )
                  ),
                  document.getElementById("root")
                )
            }
        }
      )
  }
}
