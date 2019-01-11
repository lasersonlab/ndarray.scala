package org.lasersonlab.uri.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer

trait Test {
  implicit val _system = ActorSystem()
  implicit val _mat = ActorMaterializer()
  implicit val _http = Http()
}
