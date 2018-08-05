package org.lasersonlab.ndarray

import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.Array.Aux

trait Array[T] {
  type Idx
  def shape: Idx
  def apply(idx: Idx): T

  def map[B](f: T ⇒ B): Aux[B, Idx] = {
    val self = this
    type I = Idx
    val s = shape
    def a = apply _
    new Array[B] {
      type Idx = I
      val shape = s
      def apply(idx: Idx): B = f(a(idx))
    }
  }
}

object Array {

  type Aux[T, _Idx] = Array[T] { type Idx = _Idx }

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

  def apply[T](): Aux[T, TNil] =
    new Array[T] {
      type Idx = TNil
      val shape = TNil
      def apply(idx: Idx): T =
        throw new IndexOutOfBoundsException
    }

  implicit def apply[
    T,
    Elem,
    _Idx
  ](
    t: T
  )(
    implicit
    ev: ToArray.Aux[T, Elem, _Idx]
  ):
    Aux[Elem, _Idx] =
    new Array[Elem] {
      type Idx = _Idx
      val shape: Idx = ev.shape(t)
      def apply(idx: Idx): Elem = ev(t, idx)
    }
}
