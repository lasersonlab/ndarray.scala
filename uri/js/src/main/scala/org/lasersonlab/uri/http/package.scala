package org.lasersonlab.uri

import scala.concurrent.ExecutionContext

package object http {
  case class Config(ec: ExecutionContext)
  object Config {
    implicit def toExecutionContext(config: Config): ExecutionContext = config.ec
    trait implicits {
      implicit def deriveExecutionContext(implicit config: Config): ExecutionContext = config.ec
    }
    object implicits extends implicits
    implicit def wrap(
      implicit
      ec: ExecutionContext,
    ):
      Config =
      Config(ec)
  }
}
