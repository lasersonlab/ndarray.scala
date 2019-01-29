package org.lasersonlab.diode

import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.Callback

object action {
  type Proxy = ModelProxy[_]
  implicit class Ops(val action: Action) extends AnyVal {
    def dispatch(implicit proxy: Proxy) = proxy.dispatchCB(action)
  }
  trait syntax {
    @inline implicit def makeActionOps(action: Action): Ops = Ops(action)
    @inline implicit def actionToCallback(action: Action)(implicit proxy: Proxy): Callback = proxy.dispatchCB(action)
  }
}
