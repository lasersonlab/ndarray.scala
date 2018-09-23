package org.lasersonlab.ndarray

import cats.Functor
import hammerlab.shapeless.tlist._

trait Array[T] {
  type Idx
  def shape: Idx
  def apply(idx: Idx): T
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
      Seq(s1, s2) ++
      rest
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

  implicit def functor[Shape]: Functor[Aux[?, Shape]] =
    new Functor[Aux[?, Shape]] {
      override def map[A, B](fa: Aux[A, Shape])(f: A ⇒ B): Aux[B, Shape] = {
        type I = Shape
        new Array[B] {
          type Idx = I
          val shape = fa.shape
          def apply(idx: Idx): B = f(fa(idx))
        }
      }
    }

//  implicit val traverse: Traverse[Array] = ???
//  implicit def traverseShape[Shape]: Traverse[Aux[?, Shape]] = ???
}
