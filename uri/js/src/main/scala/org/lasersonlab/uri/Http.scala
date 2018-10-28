package org.lasersonlab.uri

import java.io.IOException
import java.net.URI

import cats.effect._
import cats.implicits._
import io.scalajs.nodejs.http.IncomingMessage
import org.scalajs.dom
import org.scalajs.dom.ext.{ Ajax, AjaxException }
import org.scalajs.dom.raw.XMLHttpRequest
import io.scalajs.npm.request.{ RequestBody, _ }
import org.lasersonlab.uri.gcp.Auth

import scala.concurrent.{ Await, ExecutionContext, Promise }
import scala.scalajs.js
import scala.scalajs.js.typedarray.Int8Array
import scala.util.{ Failure, Try }

case class Options(url: String, headers: js.Object)

case class Http[F[_]: ConcurrentEffect](uri: URI)(
  implicit
  val config: Config,
  reqConfig: http.Config/*,
  ec: ExecutionContext*/
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

//  def request[A](method: String, headers: (String, String)*)(f: (IncomingMessage, Array[Byte]) ⇒ A): F[A] = {
//    val url = uri.toString
//    val hdrs = reqConfig.headers ++ headers
//
//    import scalajs.js.Dynamic.{ global ⇒ g }
//    val req = g.require("request")
//
//    F.async[A] {
//      fn ⇒
//        println("in async")
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
//              println("in callback")
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
    val responseType = ""

    println(s"sending $method to $url (${hdrs.map { case (k, v) ⇒ s"$k: $v" }.mkString(", ")})")

    val r =
      F.async[A] {
        fn ⇒
          val req = new dom.XMLHttpRequest()

          req.onreadystatechange = {
            e: dom.Event ⇒
              if (req.readyState == 4)
                fn(
                  if ((req.status >= 200 && req.status < 300) || req.status == 304)
                    Right(f(req))
                  else
                    Left(AjaxException(req))
                )
          }
          req.open(method, url)
          req.responseType = responseType
          req.timeout = timeout
          req.withCredentials = withCredentials
          hdrs.foreach(x => req.setRequestHeader(x._1, x._2))
          println("sending")
          req.send()
          println("sent…")
      }

    println("returning async object…")
    r
  }

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    request(
      "GET",
      "Range" → s"bytes=$start-${start+size-1}"
    ) {
      //(_, body) ⇒ body
      req ⇒
        println(s"response: ${req.response} ${req.response.getClass}")
        req
        .response
        .asInstanceOf[Int8Array]
        .toArray
    }
}
