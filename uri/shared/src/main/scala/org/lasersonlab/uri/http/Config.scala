package org.lasersonlab.uri.http

import scala.concurrent.duration._
case class Config(
  headers: Map[String, String] = Map.empty,
    query: Map[String, String] = Map.empty,
  timeout: Duration = 30 seconds
)
