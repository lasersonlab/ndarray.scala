package org.lasersonlab.gcp.googleapis

import com.softwaremill.sttp._
import io.circe.generic.auto._
import org.lasersonlab.files._
import org.lasersonlab.gcp._

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
