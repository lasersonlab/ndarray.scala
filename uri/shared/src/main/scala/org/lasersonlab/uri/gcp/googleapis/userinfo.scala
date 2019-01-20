package org.lasersonlab.uri.gcp.googleapis

import com.softwaremill.sttp._
import io.circe.generic.auto._
import org.lasersonlab.uri._
import org.lasersonlab.uri.gcp._

object userinfo {
  def apply()(implicit config: Config): F[User] = {
    println(s"getting userinfo, config: $config")
    Http(uri"https://www.googleapis.com/oauth2/v1/userinfo?alt=json".toJavaUri)
      .json[User]
  }
}

case class User(
  id: String,
  name: String,
  email: ?[String],
  picture: String
)
