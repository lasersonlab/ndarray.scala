package org.lasersonlab.gcp

import org.lasersonlab.gcp.oauth.{ Auth, Params }
import org.lasersonlab.uri.http

case class Config(auth: Auth, httpConfig: http.Config)
object Config {
  implicit def derive(implicit auth: Auth, httpConfig: http.Config): Config = Config(auth, httpConfig)
  trait implicits {
    implicit def configToHttpConfig(implicit config: Config): http.Config = config.httpConfig
    implicit def configToParams(implicit config: Config): Params = config.auth.params
  }
  object implicits extends implicits
}
