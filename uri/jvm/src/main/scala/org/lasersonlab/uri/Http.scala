package org.lasersonlab.uri

import java.io.IOException
import java.net.URI

import akka.http.javadsl.model.headers.ContentLength
import akka.http.scaladsl.model.HttpHeader.ParsingResult
import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok
import akka.http.scaladsl.model.HttpMethods.{ GET, HEAD }
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import cats.implicits._
import org.lasersonlab.uri.caching.Config
import org.lasersonlab.uri.http.{ Base, Defaults }
import org.lasersonlab.{ uri ⇒ u }

import scala.concurrent.Future
import scala.util.{ Failure, Success, Try }

case class Http(uri: URI)(
  implicit
  val cachingConfig: caching.Config,
  defaults: Defaults,
  httpConfig: http.Config
)
extends Uri()(httpConfig.ec)
   with Base {

  require(uri.getScheme == "http" || uri.getScheme == "https")

  type Self = Http

  implicit val u.http.Config(_, http, mat) = httpConfig
  implicit override val u.http.Config(ec, _, _) = httpConfig

  override def make(uri: URI): Http = Http(uri)

  lazy val sizeOpt: F[Try[Long]] =
    request(HEAD) {
      response ⇒
        F {
          if (response.status.isSuccess)
            Failure(
              new IOException(
                s"Error (${response.status.intValue()}) from HEAD response for $uri: ${response.status.reason}"
              )
            )
          else
            response
              .header[ContentLength]
              .fold[Try[Long]](
                Failure(
                  new IOException(
                    s"No Content-Length found in HEAD response for $uri"
                  )
                )
              )(
                h ⇒
                  Success(
                    h.value.toLong
                  )
              )
        }
    }

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    request(
      GET,
      "Range" → s"bytes=$start-${start+size-1}"
    ) {
      response ⇒
        Unmarshal(response.entity).to[Array[Byte]]
    }

  def request[A](method: HttpMethod, headers: (String, String)*)(fn: HttpResponse ⇒ F[A]): F[A] = {
    headers
      .map {
        case (k, v) ⇒
          HttpHeader.parse(k, v)
      }
      .foldRight(
        (
          List[HttpHeader](),
          List[ParsingResult]()
        )
      ) {
        case (result, (headers, errors)) ⇒
          result match {
            case Ok(header, Nil) ⇒ (header :: headers, errors)
            case error ⇒ (headers, error :: errors)
          }
      } match {
      case (_, errors @ error :: _) ⇒
        Future.failed(
          new IllegalArgumentException(
            s"Errors parsing HTTP headers: ${errors.mkString(",")}"
          )
        )
      case (headers, Nil) ⇒
        val request =
          HttpRequest(
            uri = uri.toString,
            headers = headers
          )

        http
          .singleRequest(request)
          .flatMap(fn)
    }
  }
}
