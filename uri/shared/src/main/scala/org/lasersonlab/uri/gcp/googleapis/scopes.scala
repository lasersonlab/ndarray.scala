package org.lasersonlab.uri.gcp.googleapis

import org.lasersonlab.uri.gcp.googleapis

case class Scope(override val toString: String)

abstract class Named(
  implicit
  name: sourcecode.Name,
  parent: Named = null
) {
  implicit val base: Named = this

  def child(implicit name: sourcecode.Name) = Scope(s"$this.${name.value}")

  override def toString: String =
    Option(parent)
      .fold {
        name.value
      } {
        parent ⇒ s"$parent/${name.value}"
      }
}

object scopes {
  implicit val base = new Named()(googleapis.base.toString) {}
  object auth extends Named {
    object userinfo extends Named {
      val   email = child
      val profile = child
    }
    object devstorage extends Named {
      val read_only = child
    }
    object `cloud-platform` extends Named {
      val `read-only` = child
    }
  }
}
