package org.lasersonlab.uri

import shapeless._
import shapeless.ops.hlist.Modifier

trait Patch[T, E] {
  def apply(t: T, Δ: Δ[E]): T
}
object Patch {
  implicit def cc[T, L <: HList, E](implicit g: Generic.Aux[T, L], p: Lazy[Patch[L, E]]): Patch[T, E] =
    (t: T, Δ: Δ[E]) ⇒ g.from(p.value(g.to(t), Δ))

  implicit def hlist[L <: HList, E](implicit m: Modifier.Aux[L, E, E, (E, L)]): Patch[L, E] =
    (l: L, Δ: Δ[E]) ⇒ m(l, Δ)._2
}
