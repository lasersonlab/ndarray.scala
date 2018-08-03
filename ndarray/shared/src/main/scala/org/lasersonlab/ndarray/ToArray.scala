package org.lasersonlab.ndarray

import shapeless._

trait ToArray[T] {
  type Elem
  type Idx
  def shape(t: T): Idx
  def apply(t: T, idx: Idx): Elem
}

trait LowPriToArray {

  type Aux[T, _Elem, _Idx] =
    ToArray[T] {
      type Elem = _Elem
      type Idx = _Idx
    }

  def apply[T, _Elem, _Idx](
    _shape: T ⇒ _Idx,
    _apply: (T, _Idx) ⇒ _Elem
  ):
    Aux[T, _Elem, _Idx] =
    new ToArray[T] {
      type Elem = _Elem
      type Idx = _Idx
      @inline def shape(t: T): Idx = _shape(t)
      @inline def apply(t: T, idx: Idx): Elem = _apply(t, idx)
    }

  implicit def singleton[T]: Aux[T, T, HNil] =
    apply[T, T, HNil](
      t ⇒ HNil,
      (t, idx) ⇒ t
    )
}

object ToArray
  extends LowPriToArray {
  implicit def seq[
    T,
    Elem,
    _Idx <: HList
  ](
    implicit
    prev: Aux[T, Elem, _Idx]
  ):
    Aux[
      Seq[T],
      Elem,
      Int :: _Idx
    ] =
    apply(
      t ⇒ {
        val head = prev.shape(t.head)

        t
          .iterator
          .drop(1)
          .map(prev.shape)
          .find {
            _ != head
          }
          .foreach {
            shape ⇒
              throw new IllegalArgumentException(
                s"Mismatched shapes: $head, $shape"
              )
          }

        t.size :: head
      },
      (t, idx) ⇒
        idx match {
          case head :: tail ⇒
            prev(t(head), tail)
        }
  )
}
