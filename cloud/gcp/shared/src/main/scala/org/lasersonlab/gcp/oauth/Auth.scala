package org.lasersonlab.gcp.oauth

import java.time.Instant
import java.time.Instant.now

import hammerlab.either._
import hammerlab.str._
import io.circe._
import io.circe.generic.auto._
import lasersonlab.circe._
import org.lasersonlab.gcp.oauth.Auth._

case class Auth(
  token: String,
  granted: Instant,
  expires: Instant,
  scopes: Seq[String],
  params: Params,
  state: State
) {
  def expired = now().isAfter(expires)
  def expire: Option[Auth] =
    state match {
      case Valid ⇒ Some(copy(state = Expired))
      case _ ⇒ None
    }
  def fail: Option[Auth] =
    state match {
      case Failed ⇒ None
      case _ ⇒ Some(copy(state = Failed))
    }
}

object Auth {
  def fromFragment(map: Map[String, String])(implicit params: Params): String | Auth = {
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
      granted = now()
      expires ← get(  'expires_in).map { s ⇒ granted.plusSeconds(s.toInt) }
       scopes ← get(       'scope).map { _.split(",") }
    } yield
      Auth(
        token,
        granted,
        expires,
        scopes,
        params,
        Valid
      )
  }

  import io.circe.generic.semiauto._
  implicit val encoder: Encoder[Auth] = deriveEncoder
  implicit val decoder: Decoder[Auth] = deriveDecoder

  sealed trait State
  case object Valid extends State
  case object Expired extends State
  case object Failed extends State
}
