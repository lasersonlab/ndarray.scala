package org.lasersonlab.uri

import java.io.{ FileNotFoundException, IOException }
import java.net.URI

import cats.effect.Sync
import cats.implicits._
import org.scalajs.dom.ext.Ajax

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.util.Try

case class Http[F[_]: Sync](uri: URI)(
  implicit
  val config: Config,
  reqConfig: http.Config,
  ec: ExecutionContext
)
extends Uri[F] {

  require(uri.getScheme == "http" || uri.getScheme == "https")

  override def exists: F[Boolean] = sizeOpt.map { _.isSuccess }

  override def size: F[Long] =
    sizeOpt
      .map {
        _
          .toEither
          .leftMap(
            e ⇒
              new FileNotFoundException(
                s"Path not found: $uri"
              )
              .initCause(e)
          )
      }
      .rethrow

  lazy val sizeOpt =
    delay {
      import com.softwaremill.sttp._
      val u = uri"$uri".params(reqConfig.query)
      Try {
        Await.result(
          Ajax(
            "HEAD",
            uri"$uri"
              .params(reqConfig.query)
              .toString,
            data = null,
            timeout = 0,
            headers = reqConfig.headers,
            withCredentials = false,
            responseType = ""
          )
          .flatMap {
            req ⇒
              req.getResponseHeader("Content-Length") match {
                case null | "" ⇒ Future.failed(new IOException(s"No Content-Length header found in HEAD response for $u"))
                case s ⇒ Future.successful(s.toLong)
              }
          },
          reqConfig.timeout
        )
      }
    }

  override def bytes(start: Long, size: Int): F[Array[Byte]] = ???
}
