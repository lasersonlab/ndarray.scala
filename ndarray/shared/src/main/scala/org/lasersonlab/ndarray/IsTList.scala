package org.lasersonlab.ndarray

import shapeless._, nat._
import TList.{ Base, Cons }

/**
 * Type-class converting a type [[T]] to a corresponding [[TList]] of [[N]] [[Elem elements]]
 */
trait IsTList[T, Elem, N <: Nat] {
  def apply(t: T): TList[Elem, N]
}

trait LowPriIsTlist {

  /**
   * Constructor short-hand
   */
  def apply[T, Elem, N <: Nat](fn: T ⇒ TList[Elem, N]): IsTList[T, Elem, N] =
    new IsTList[T, Elem, N] {
      def apply(t: T): TList[Elem, N] = fn(t)
    }

  /**
   * Fallback: every [[T]] can be a [[TList]] of [[_1 one]] [[T]]
   */
  implicit def singleton[T]: IsTList[T, T, _1] = apply(Base(_))
}

object IsTList
  extends LowPriIsTlist {

  /**
   * A one-element [[HList]] is a [[TList]] of one [[T]]
   */
  implicit def baseHList[T]: IsTList[T :: HNil, T, _1] =
    apply(
      l ⇒ Base(l.head)
    )

  /**
   * Prepend an element to a [[TList]]-able [[HList]]
   *
   * @param tail turn an [[L]] into a [[TList]] of [[N]] [[T]]s
   * @tparam T element-type
   * @tparam L [[HList]]-type made of [[N]] [[T]]s
   * @tparam N length of [[L]]
   * @return [[IsTList]] instance for creating a [[TList]] of [[T]]s one longer than [[N]] from [[HList]]s comprised of
   *         a [[T]] prepended to an [[L]]
   */
  implicit def consHList[
    T,
    L <: HList,
    N <: Nat
  ](
    implicit
    tail: Lazy[IsTList[L, T, N]]
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

  /**
   * If a [[Product]]'s [[Generic]] form is a [[TList]] (of [[N]] [[Elem elements]]), then so is that [[Product]]
   *
   * Importantly: derives instances for tuples
   *
   * It will also derive instances for suitably-shaped case-classes (where all fields are the same type); unclear
   * whether that's a feature or a bug.
   *
   * @param g [[Generic]] evidence linking [[T]] to [[L]]
   * @param ev evidence that the generic form of [[T]], [[L]], is a [[TList]] of [[N]] [[Elem]]s
   * @tparam T input / [[Product]] type
   * @tparam L corresponding generic / [[HList]] type
   * @tparam Elem underlying element-type comprising the [[TList]]
   * @tparam N number of [[Elem]]s in the [[TList]] / [[T]] / [[L]]
   * @return [[IsTList]] instance for [[Product]]-type [[T]]
   */
  implicit def product[
    T <: Product,
    L <: HList,
    Elem,
    N <: Nat
  ](
    implicit
    g: Generic.Aux[T, L],
    ev: IsTList[L, Elem, N]
  ):
    IsTList[T, Elem, N] =
    apply(
      t ⇒ ev(g.to(t))
    )
}
