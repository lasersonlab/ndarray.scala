package org.lasersonlab.uri

import java.net.URLEncoder

object encode {
  def apply(str: String): String = URLEncoder.encode(str, "UTF-8")
}
