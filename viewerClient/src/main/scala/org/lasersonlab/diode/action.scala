package org.lasersonlab.diode

import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.Callback

object action {
  implicit class Ops(val action: Action) extends AnyVal {
    def dispatch(implicit model: ModelProxy[_]) = model.dispatchCB(action)
  }
  trait syntax {
    @inline implicit def makeActionOps(action: Action): Ops = Ops(action)
    @inline implicit def actionToCallback(action: Action)(implicit model: ModelProxy[_]): Callback = model.dispatchCB(action)
  }
}
