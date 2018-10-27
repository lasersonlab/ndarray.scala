package org.lasersonlab.uri

import java.io.IOException
import java.net.URI

import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._

import scala.util.{ Failure, Success, Try }

case class Http[F[_]: ConcurrentEffect](uri: URI)(
  implicit
  val config: Config,
  reqConfig: http.Config
)
extends Uri[F]
with Http4sDsl[F]
with Http4sClientDsl[F] {

  import scala.concurrent.ExecutionContext.Implicits.global
  val client = BlazeClientBuilder[F](global).resource.use { sync.pure }

  override def exists: F[Boolean] = sizeOpt.map { _.isSuccess }

  override def size: F[Long] = sizeOpt.map { _.toEither }.rethrow

  lazy val sizeOpt: F[Try[Long]] =
    for {
      uri ←
        delay[Either[Throwable,org.http4s.Uri]] {
          Uri.fromString(uri.toString)
        }
        .rethrow
      request ←
        HEAD(
          uri,
          reqConfig.headers.toList.map { case (k, v) => Header(k, v) }: _*
        )
      client ← client
      response ←
        client
          .fetch[Try[Long]](request) {
            response ⇒
              delay {
                if (response.status.isSuccess)
                  Failure(new IOException(s"Error (${response.status.code}) from HEAD response for $uri: ${response.status.reason}"))
                else
                  response
                    .headers
                    .get(`Content-Length`)
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
    } yield
      response

  override def bytes(start: Long, size: Int): F[Array[Byte]] = ???
}
