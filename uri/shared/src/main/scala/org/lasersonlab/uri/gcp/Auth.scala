package org.lasersonlab.uri.gcp

import java.time.Instant
import java.time.Instant.now

import hammerlab.either._
import hammerlab.option._
import hammerlab.str._

case class Auth(
  token: String,
  expires: Long,
  scopes: Seq[String],
  project: Option[String] = None
)

object Auth {
  def fromFragment(map: Map[String, String]): String | Auth = {
    def get(key: String) =
      map
        .get(key)
        .fold[
          String | String
        ] {
          Left(s"Missing key: $key")
        } {
          Right(_)
        }

    for {
        token ← get('access_token)
      expires ← get(  'expires_in).map { s ⇒ now().plusSeconds(s.toInt) }
       scopes ← get(       'scope).map { _.split(",") }
    } yield
      Auth(
        token,
        expires.getEpochSecond,
        scopes
      )
  }
}
