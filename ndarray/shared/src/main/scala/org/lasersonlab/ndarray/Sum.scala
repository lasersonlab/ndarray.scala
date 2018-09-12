package org.lasersonlab.ndarray

import cats.Monoid
import hammerlab.shapeless.tlist._
import shapeless.Lazy

trait Sum[In] {
  type Out
  def apply(in: In): Out
}
object Sum {
  type Aux[T, _O] = Sum[T] { type Out = _O }

  implicit def seq[T](implicit m: Monoid[T]): Aux[Seq[T], T] =
    new Sum[Seq[T]] {
      type Out = T
      def apply(in: Seq[T]): T = in.foldLeft(m.empty)(m.combine)
    }

  implicit def tnil[T](implicit m: Monoid[T]): Aux[TNil, T] =
    new Sum[TNil] {
      type Out = T
      def apply(t: TNil): T = m.empty
    }

  implicit def cons[
    E,
    T <: TList
  ](
    implicit
    p: Lazy[Aux[T, E]],
    m: Monoid[E],
    pp: Prepend[E, T]
  ):
    Aux[
      E :: T,
      E
    ] =
    new Sum[E :: T] {
      type Out = E
      def apply(in: E :: T): E =
        m.combine(
          in.head,
          p.value(
            in.tail
          )
        )
    }
}
