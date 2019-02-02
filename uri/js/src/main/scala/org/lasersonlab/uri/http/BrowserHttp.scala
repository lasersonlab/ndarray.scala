package org.lasersonlab.uri.http

import java.io.IOException
import java.net.URI

import org.lasersonlab.{ uri ⇒ u }
import org.lasersonlab.uri.{ Http, Uri, caching, http }
import org.scalajs.dom
import org.scalajs.dom.ext.AjaxException
import org.scalajs.dom.raw.XMLHttpRequest

import scala.concurrent.Promise
import scala.scalajs.js.typedarray.{ ArrayBuffer, TypedArrayBuffer }
import scala.util.{ Failure, Try }

case class BrowserHttp(uri: URI)(
    implicit
    val cachingConfig: caching.Config,
    defaults: Defaults,
    httpConfig: http.Config
)
extends Uri()(httpConfig)
   with http.Base
   with Http {

  type Self = BrowserHttp

  def make(uri: URI): Self = BrowserHttp(uri)

  lazy val sizeOpt =
    request[Try[Long]]("HEAD") {
      _
        .getResponseHeader("Content-Length") match {
          case null | "" ⇒ Failure(new IOException(s"No Content-Length header found in HEAD response for $uri"))
          case s ⇒ Try { s.toLong }
        }
    }

  private def request[A](method: String, headers: (String, String)*)(f: XMLHttpRequest ⇒ A): F[A] = {
    val url = uri.toString
    val timeout = 0
    val hdrs = defaults.headers ++ headers
    val withCredentials = false
    val responseType = "arraybuffer"

    logger.debug(s"sending $method to $url (${hdrs.map { case (k, v) ⇒ s"$k: $v" }.mkString(", ")})")

    val p = Promise[A]()

    val req = new dom.XMLHttpRequest()

    req.onreadystatechange = {
      e: dom.Event ⇒
        if (req.readyState == 4)
          if ((req.status >= 200 && req.status < 300) || req.status == 304)
            p.success(f(req))
          else {
            p.failure(AjaxException(req))
          }
    }
    req.open(method, url)
    req.responseType = responseType
    req.timeout = timeout
    req.withCredentials = withCredentials
    hdrs.foreach(x => req.setRequestHeader(x._1, x._2))
    req.send()

    p.future
  }

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    request(
      "GET",
      "Range" → s"bytes=$start-${start+size-1}"
    ) {
      req ⇒
        val arraybuffer =
          req
            .response
            .asInstanceOf[ArrayBuffer]
        val size = arraybuffer.byteLength

        val bytes = Array.fill[Byte](size)(0)

        TypedArrayBuffer
          .wrap(arraybuffer)
          .get(bytes)

        logger.debug(s"uri response for $uri ($start + $size; resp type ${req.responseType}, class ${req.response.getClass}): ${arraybuffer.byteLength} (${bytes.length}) bytes, type ${req.getResponseHeader("Content-Type")}")

        bytes
    }
}
