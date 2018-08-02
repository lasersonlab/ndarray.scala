package org.lasersonlab.ndarray.fresh

import shapeless.{ nat, _ }
import nat._
import org.lasersonlab.ndarray.fresh.TList.{ Base, Cons }
import shapeless.ops.hlist.Length

trait IsTList[T, Elem, N <: Nat] {
  def apply(t: T): TList[Elem, N]
}
trait LowPriIsTlist {

  def apply[T, Elem, N <: Nat](fn: T ⇒ TList[Elem, N]): IsTList[T, Elem, N] =
    new IsTList[T, Elem, N] {
      def apply(t: T): TList[Elem, N] = fn(t)
    }

  implicit def singleton[T]: IsTList[T, T, _1] = apply(Base(_))
}
object IsTList
  extends LowPriIsTlist {
  implicit def baseHList[T]: IsTList[T :: HNil, T, _1] =
    apply(
      l ⇒ Base(l.head)
    )

  implicit def consHList[
    T,
    L <: HList,
    N <: Nat
  ](
    implicit
    tail: Lazy[IsTList[L, T, N]],
    len: Length.Aux[L, N]
  ):
    IsTList[
      T :: L,
      T,
      Succ[N]
    ] =
    apply(
      l ⇒
        Cons(
          l.head,
          tail.value(l.tail)
        )
    )

  implicit def product[
    T <: Product,
    L <: HList,
    Elem,
    N <: Nat
  ](
    implicit
    g: Generic.Aux[T, L],
    len: Length.Aux[L, N],
    ev: IsTList[L, Elem, N]
  ):
    IsTList[T, Elem, N] =
    apply(
      t ⇒ ev(g.to(t))
    )
}

sealed trait TList[T, N <: Nat] {
  def head: T
}
object TList {
  case class Base[T](head: T) extends TList[T, _1]
  object Base {
    implicit def wrap[T](t: T): Base[T] = Base(t)
  }
  case class Cons[T, P <: Nat](head: T, tail: TList[T, P]) extends TList[T, Succ[P]]

  implicit def wrap[T, Elem, N <: Nat](t: T)(implicit ev: IsTList[T, Elem, N]): TList[Elem, N] = ev(t)
}
