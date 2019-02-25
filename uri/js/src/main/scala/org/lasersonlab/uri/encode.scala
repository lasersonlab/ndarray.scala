package org.lasersonlab.uri

import scala.scalajs.js.URIUtils

object encode {
  def apply(str: String): String = URIUtils.encodeURI(str)
}
