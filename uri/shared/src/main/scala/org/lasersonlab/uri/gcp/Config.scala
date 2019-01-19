package org.lasersonlab.uri.gcp

import org.lasersonlab.uri.http

case class Config(auth: Auth, httpConfig: http.Config)
object Config {
  implicit def derive(implicit auth: Auth, httpConfig: http.Config): Config = Config(auth, httpConfig)
  trait implicits {
    implicit def configToHttpConfig(implicit config: Config): http.Config = config.httpConfig
  }
  object implicits extends implicits
}
