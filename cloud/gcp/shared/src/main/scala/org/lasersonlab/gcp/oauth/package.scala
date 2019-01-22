package org.lasersonlab.gcp

package object oauth {
  case class Scopes(scopes: Scope*) {
    override def toString: String = scopes.mkString(" ")
  }

  case class ClientId(override val toString: String)

  case class RedirectUrl(override val toString: String)
}
