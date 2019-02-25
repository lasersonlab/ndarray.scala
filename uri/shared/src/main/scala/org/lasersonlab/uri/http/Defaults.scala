package org.lasersonlab.uri.http

import scala.concurrent.duration._
case class Defaults(
  headers: Map[String, String] = Map.empty,
  timeout: Duration = 30 seconds
)
object Defaults {
  implicit val default = Defaults()
}
