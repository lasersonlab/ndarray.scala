package org.lasersonlab.ndarray

trait Traverse[T, Elem] {
  def apply(t: T): Iterator[Elem]
}
object Traverse {
  implicit def naiveArray[T, Elem, Idx <: TList](
    implicit
    ta: ToArray.Aux[T, Elem, Idx],
    ti: TraverseIndices[Idx]
  ):
    Traverse[T, Elem] =
    new Traverse[T, Elem] {
      def apply(t: T): Iterator[Elem] =
        ti(ta.shape(t)).map(idx ⇒ ta(t, idx))
    }
}
