package org.lasersonlab.uri.http

import java.io.IOException
import java.net.URI

import cats.implicits._
import io.scalajs.nodejs.http.IncomingMessage
import io.scalajs.npm.request._
import org.lasersonlab.{ uri ⇒ u }
import org.lasersonlab.uri.{ Http, Uri, caching, http }

import scala.concurrent.Promise
import scala.scalajs.js.Dictionary
import scala.scalajs.js.typedarray.{ ArrayBuffer, TypedArrayBuffer }
import scala.util.{ Failure, Try }

case class NodeHttp(uri: URI)(
    implicit
    val cachingConfig: caching.Config,
    defaults: Defaults,
    httpConfig: http.Config
)
extends Uri()(httpConfig)
   with http.Base
   with Http {

  type Self = NodeHttp
  import NodeHttp.req

  def make(uri: URI): Self = NodeHttp(uri)

  lazy val sizeOpt =
    request[Try[Long]]("HEAD") {
      (response, body) ⇒
        response.headers.get("content-length") match {
          case None ⇒
            Failure(
              new IOException(
                s"No Content-Length header found in HEAD response for $uri"
              )
            )
          case Some(s) ⇒
            Try {
              s.toLong
            }
        }
    }

  // Attempt at using Node `request` module
  private def request[A](method: String, headers: (String, String)*)(f: (IncomingMessage, Array[Byte]) ⇒ A): F[A] = {
    val url = uri.toString
    val hdrs = defaults.headers ++ headers
    logger.debug(s"$uri: requesting ($method)${if (headers.nonEmpty) s": ${headers.map{case (k,v)⇒s"$k: $v"}.mkString(", ")}" else ""}")
    val p = Promise[A]()

    req(
      Dictionary(
        "url" → url,
        "method" → method,
        "headers" → Dictionary(hdrs.toSeq: _*),
        "encoding" → null
      ),
      {
        (
          error: RequestError,
          response: IncomingMessage,
          body: RequestBody
        ) ⇒
          if (error == null) {
            val buffer = TypedArrayBuffer.wrap(body.asInstanceOf[ArrayBuffer])
            val length = buffer.remaining()
            val bytes = Array.fill(length)(0.toByte)
            buffer.get(bytes)
            logger.debug(s"$url ($method): status ${response.statusCode}, ${bytes.length} bytes, headers: ${response.headers.toVector.map { case (k, v) ⇒ s"$k: $v" }.mkString(",")}")
            p.success(
              f(
                response,
                bytes
              )
            )
          }
          else {
            logger.warn(s"$uri: failure: $error")
            p.failure(error)
          }
      }
    )

    p.future
  }

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    request(
      "GET",
      "Range" → s"bytes=$start-${start+size-1}"
    ) {
      (_, body) ⇒ body
    }
}

object NodeHttp {
  import scalajs.js.Dynamic.{ global ⇒ g }
  val req = g.require("request")

}
