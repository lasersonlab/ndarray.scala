package org.lasersonlab.ndview

import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._

package object view {
  type Router = RouterCtl[Route]
  def cls(k: String, c: String = null) =
    TagMod(
      key := k,
      className := Option(c).getOrElse(k)
    )
}
