package org.lasersonlab.ndarray

import shapeless.{ HList, _ }
import shapeless.{ Nat, Succ }
import org.hammerlab.shapeless.tlist.TList.{ Base, Cons }
//import shapeless.ops.tuple.Prepend

trait ToArray[T] {
  type Elem
  type Idx
  def shape(t: T): Idx
  def apply(t: T, idx: Idx): Elem
}

//trait Head[T] {
//  type Out
//  def apply(t: T): Out
//}
//trait LowPriHead {
//  type Aux[T, _O] = Head[T] { type Out = _O }
//  def apply[T, _O](fn: T ⇒ _O): Aux[T, _O] =
//    new Head[T] {
//      type Out = _O
//      @inline def apply(t: T): _O = fn(t)
//    }
//
//  implicit def id[T]: Aux[T, T] = apply(x ⇒ x)
//}
//object Head
//  extends LowPriHead {
//  implicit def generic[H, L <: HList]: Aux[H :: L, H] = apply(_.head)
//  implicit def product[T <: Product, L <: HList](implicit g: Generic.Aux[T, L], l: Head[L]): Aux[T, l.Out] =
//    apply(t ⇒ l(g.to(t)))
//}

//case object Empty

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

//trait Prepend[H, T] {
//  type Out
//  def apply(h: H, t: T): Out
//  def apply(out: Out): (H, T)
//}
//trait LowPriPrepend {
//  type Aux[H, T, _Out] = Prepend[H, T] { type Out = _Out }
//  def apply[H, T, _Out](to: (H, T) ⇒ _Out, from: _Out ⇒ (H, T)): Aux[H, T, _Out] =
//    new Prepend[H, T] {
//      type Out = _Out
//      @inline def apply(h: H, t: T): _Out = to(h, t)
//      @inline def apply(out: _Out): (H, T) = from(out)
//    }
////  implicit def base[H]: Aux[H, Empty.type, H]  = apply((h, _) ⇒ h, h ⇒ (h, Empty))
//}
//object Prepend
//  extends LowPriPrepend {
//  implicit def singletonHList[H]: Aux[H, HNil.type, H :: HNil] =
//    apply(
//      (h, _) ⇒ h :: HNil,
//      l ⇒ (l.head, HNil)
//    )
//
//  implicit def cons[H, L <: HList, M <: HList](implicit l: Aux[H, M, L]): Aux[H, L, H :: L] =
//    apply(
//      (h, l) ⇒ h :: l,
//      l ⇒ (l.head, l.tail)
//    )
//
//  implicit def generic[T, L <: HList]
//}

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
