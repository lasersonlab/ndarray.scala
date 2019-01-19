package org.lasersonlab.uri

import java.net.URI

import org.lasersonlab.uri.caching.Config
import org.lasersonlab.uri.http.{ BrowserHttp, Defaults, NodeHttp }

trait Http extends Uri
object Http {
  def apply(uri: URI)(
    implicit
    config: Config,
    defaults: Defaults,
    httpConfig: http.Config
  ): Http =
    if (js.node_?)
      NodeHttp(uri)
    else
      BrowserHttp(uri)
}
