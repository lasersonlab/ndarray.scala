package org.lasersonlab.uri

import java.io.IOException
import java.net.URI

import cats.effect.Sync
import cats.implicits._
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.XMLHttpRequest

import scala.concurrent.{ Await, ExecutionContext }
import scala.util.{ Failure, Try }

case class Http[F[_]: Sync](uri: URI)(
  implicit
  val config: Config,
  reqConfig: http.Config,
  ec: ExecutionContext
)
extends Uri[F]
   with http.Base[F] {

  type Self = Http[F]

  def make(uri: URI): Http[F] = Http(uri)

  import com.softwaremill.sttp._
  val u = uri"$uri".params(reqConfig.query)

  lazy val sizeOpt =
    request[Try[Long]]("HEAD") {
      _
        .getResponseHeader("Content-Length") match {
          case null | "" ⇒ Failure(new IOException(s"No Content-Length header found in HEAD response for $u"))
          case s ⇒ Try { s.toLong }
        }
    }

  def request[A](method: String, headers: (String, String)*)(fn: XMLHttpRequest ⇒ A): F[A] =
    delay {
      Await.result(
        Ajax(
          method,
          uri"$uri"
            .params(reqConfig.query)
            .toString,
          data = null,
          timeout = 0,
          headers = reqConfig.headers ++ headers,
          withCredentials = false,
          responseType = ""
        ),
        reqConfig.timeout
      )
    }
    .map(fn)

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    request(
      "GET",
      "Range" → s"bytes=$start-${start+size-1}"
    ) {
      _.response.asInstanceOf[Array[Byte]]
    }
}
