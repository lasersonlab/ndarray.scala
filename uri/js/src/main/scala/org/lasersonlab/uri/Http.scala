package org.lasersonlab.uri

import java.io.IOException
import java.net.URI

import cats.effect._
import cats.implicits._
import org.scalajs.dom
import org.scalajs.dom.ext.AjaxException
import org.scalajs.dom.raw.XMLHttpRequest

import scala.scalajs.js
import scala.scalajs.js.typedarray.{ ArrayBuffer, TypedArrayBuffer }
import scala.util.{ Failure, Try }

case class Http[F[_]: ConcurrentEffect](uri: URI)(
  implicit
  val config: Config,
  reqConfig: http.Config
)
extends Uri[F]
   with http.Base[F] {

  val F = ConcurrentEffect[F]

  type Self = Http[F]

  def make(uri: URI): Http[F] = Http(uri)

  lazy val sizeOpt =
    request[Try[Long]]("HEAD") {
      _
        .getResponseHeader("Content-Length") match {
        case null | "" ⇒ Failure(new IOException(s"No Content-Length header found in HEAD response for $uri"))
        case s ⇒ Try { s.toLong }
      }
//      (response, _) ⇒
//        response.headers("Content-Length") match {
//          case null | "" ⇒ Failure(new IOException(s"No Content-Length header found in HEAD response for $uri"))
//          case s ⇒ Try { s.toLong }
//        }
    }

  // Attempt at using Node `request` module
//  def request[A](method: String, headers: (String, String)*)(f: (IncomingMessage, Array[Byte]) ⇒ A): F[A] = {
//    val url = uri.toString
//    val hdrs = reqConfig.headers ++ headers
//
//    import scalajs.js.Dynamic.{ global ⇒ g }
//    val req = g.require("request")
//
//    F.async[A] {
//      fn ⇒
//        req(
//          js.Dictionary(
//            "url" → url,
//            "headers" →
//              js.Dictionary(
//                hdrs.toSeq: _*
//              )
//          ),
//          {
//            (
//              error: RequestError,
//              response: IncomingMessage,
//              body: RequestBody
//            ) ⇒
//              fn(
//                if (error == null)
//                  Right(
//                    f(
//                      response,
//                      body.asInstanceOf[Int8Array].toArray
//                    )
//                  )
//                else
//                  Left(error)
//              )
//          }
//        )
//    }
//  }

  def request[A](method: String, headers: (String, String)*)(f: XMLHttpRequest ⇒ A): F[A] = {
    val url = uri.toString
    val timeout = 0
    val hdrs = reqConfig.headers ++ headers
    val withCredentials = false
    val responseType = "arraybuffer"

    logger.debug(s"sending $method to $url (${hdrs.map { case (k, v) ⇒ s"$k: $v" }.mkString(", ")})")

    F.async[A] {
      fn ⇒
        val req = new dom.XMLHttpRequest()

        req.onreadystatechange = {
          e: dom.Event ⇒
            if (req.readyState == 4)
              fn(
                if ((req.status >= 200 && req.status < 300) || req.status == 304)
                  Right(f(req))
                else {
                  Left(AjaxException(req))
                }
              )
        }
        req.open(method, url)
        req.responseType = responseType
        req.timeout = timeout
        req.withCredentials = withCredentials
        hdrs.foreach(x => req.setRequestHeader(x._1, x._2))
        req.send()
    }
  }

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    request(
      "GET",
      "Range" → s"bytes=$start-${start+size-1}"
    ) {
      //(_, body) ⇒ body
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
