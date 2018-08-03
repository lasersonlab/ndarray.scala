package org.lasersonlab.ndarray

trait Array[T] {
  type Idx
  def shape: Idx
  def apply(idx: Idx): T
}

object Array {
  implicit def fromToArray[T, Elem, _Idx](t: T)(implicit ev: ToArray.Aux[T, Elem, _Idx]): Array[Elem] =
    new Array[Elem] {
      type Idx = _Idx
      val shape: Idx = ev.shape(t)
      def apply(idx: Idx): Elem = ev(t, idx)
    }
}
