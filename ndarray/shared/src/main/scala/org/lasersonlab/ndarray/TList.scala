package org.lasersonlab.ndarray

import org.lasersonlab.ndarray

sealed trait TList {
  type T
}
object TList {
  type Aux[_T] = TList { type T = _T }
  implicit class Ops[L <: TList](val l: L) extends AnyVal {
    def ::[T](t: T)(implicit ev: Prepend[T, L]) = ev(t, l)
  }
}

sealed trait TNil extends TList {
  type T = Nothing
  def ::[T](t: T) = ndarray.::(t, this)
}
case object TNil extends TNil

case class ::[_T, Tail <: TList](head: _T, tail: Tail) extends TList {
  type T = _T
  def ::[U >: this.type <: TList.Aux[T]](t: T): ::[T, U] = ndarray.::(t, this: U)
}
