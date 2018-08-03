package org.lasersonlab.ndarray

trait Array[T] {
  type Idx
  def shape: Idx
  def apply(idx: Idx): T
}

object Array {

  def apply[S, Elem, Idx](
    s1: S,
    s2: S,
    rest: S*
  )(
    implicit
    ev: ToArray.Aux[Seq[S], Elem, Idx]
  ):
    Array[Elem] =
    apply(
      Seq(s1, s2) ++ rest
    )

  implicit def apply[T, Elem, _Idx](t: T)(implicit ev: ToArray.Aux[T, Elem, _Idx]): Array[Elem] =
    new Array[Elem] {
      type Idx = _Idx
      val shape: Idx = ev.shape(t)
      def apply(idx: Idx): Elem = ev(t, idx)
    }
}
