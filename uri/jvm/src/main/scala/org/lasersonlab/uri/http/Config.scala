package org.lasersonlab.uri.http

import akka.http.scaladsl.HttpExt
import akka.stream.Materializer

import scala.concurrent.ExecutionContext

case class Config(
  ec: ExecutionContext,
  http: HttpExt,
  mat: Materializer
)
object Config {
  implicit def toExecutionContext(config: Config): ExecutionContext = config.ec
  trait implicits {
    implicit def deriveExecutionContext(implicit config: Config): ExecutionContext = config.ec
  }
  object implicits extends implicits
  implicit def wrap(
    implicit
    ec: ExecutionContext,
    http: HttpExt,
    mat: Materializer
  ): Config =
    Config(
      ec,
      http,
      mat
    )
}
