package org.lasersonlab

import org.lasersonlab.files.http

package object gcp {
  implicit def httpDefaults(implicit config: gcp.Config): http.Defaults =
    http.Defaults(
      headers =
        Map(
          "Authorization" → s"Bearer ${config.auth.token}"
        )
    )
}
