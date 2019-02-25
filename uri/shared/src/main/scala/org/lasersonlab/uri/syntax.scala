package org.lasersonlab.uri

import java.net.URI

trait syntax {
  import syntax.Ops
  @inline implicit def makeURIOps(uri: URI): Ops = Ops(uri)
  @inline implicit def sttpToJavaNetUri(uri: com.softwaremill.sttp.Uri): URI = uri.toJavaUri
}

object syntax {
  implicit class Ops(val uri: URI) extends AnyVal {
    def path: Vector[String] =
      uri
        .normalize
        .getPath
        .replaceAll  ("/+", "/")  // collapse repeated '/'s
        .replaceFirst("/$", "" )  // remove trailing '/'
        .split("/")
        .toVector

    def parentOpt: Option[URI] =
      path match {
        case Vector() ⇒ None
        case parent :+ _ ⇒
          Some(
            new URI(
              uri.getScheme,
              uri.getAuthority,
              parent.mkString("/"),
              uri.getQuery,
              uri.getFragment
            )
          )
      }
  }
}
