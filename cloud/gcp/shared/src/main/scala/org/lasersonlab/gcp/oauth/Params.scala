package org.lasersonlab.gcp.oauth

case class Params(
  clientId: ClientId,
  redirectUrl: RedirectUrl,
  scopes: Scopes
) {
  def map =
    Map(
      "client_id" → clientId,
      "redirect_uri" → redirectUrl,
      "scope" → scopes,
      "include_granted_scopes" → "true",
      "response_type" → "token"
    )
}
object Params {
  implicit def wrap(
    implicit
    clientId: ClientId,
    redirectUrl: RedirectUrl,
    scopes: Scopes
  ) =
    Params(
      clientId,
      redirectUrl,
      scopes
    )
}

