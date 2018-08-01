package org.lasersonlab.shapeless

import hammerlab.shapeless._
import org.lasersonlab.shapeless.TList.Cons
import shapeless._
import shapeless.Nat._
import shapeless.ops.nat.ToInt

trait Pred[N <: Nat] {
  type P <: Nat
  def p: P
}
object Pred {

  implicit val __0 = _0

  implicit def fromSucc[_P <: Nat](implicit _p: _P): Pred.Aux[Succ[_P], _P] =
    new Pred[Succ[_P]] {
      type P = _P
      def p = _p
    }

  type Aux[N <: Nat, _P <: Nat] = Pred[N] { type P = _P }

//  fromSucc[_1]
//  implicitly[Pred[_1]]
//  implicitly[Pred.Aux[_1, _0]]
//
//  fromSucc[_2]
//  implicitly[_2 =:= Succ[_1]]
//  implicitly[Pred[_2]]
//  implicitly[Pred.Aux[_2, _1]]
//
//  fromSucc[_3]
//  implicitly[_3 =:= Succ[_2]]
//  implicitly[Pred[_3]]
//  implicitly[Pred.Aux[_3, _2]]
}

/**
 * Collection of objects, with the size incorporated into the type-system
 *
 * Notable members:
 *
 * - `head`: first value in the list
 * - `tail`: [[TList]] with all elements after `head`
 * - `size`: number of elements
 */
sealed abstract class TList[T] {
  def head: T

  // type-level size
  type Size <: Nat
  protected implicit def _size: Size

  def size: Int

  /**
   * should be one less than [[Size]], i.e. Size =:= Succ[Prev]; enforcement left to
   * implementations
   */
  type Prev <: Nat
  implicit def ev: Size =:= Succ[Prev]
  type Tail = TList.Of[T, Prev]
  def tail: Option[Tail]
//  def tail[Prev <: Nat](implicit pred: Pred.Aux[Size, Prev]): Option[TList.Of[T, Prev]]

  /**
   * Prepend an element to this [[TList]]
   *
   * The returned [[Cons]] can store its [[Tail]]-type anywhere between the specific `this.type`
   * and the upper-bound `TList.Of[T, Size]`
   */
  def prepend(
    t: T
  )(
    implicit
    n: Succ[Size],
    ti: ToInt[Succ[Size]]
  ):
    Cons[T, Size] =
    Cons[T, Size](
      t,
      this
    )
}

object TList {

  /** Can implicitly convert a [[TList]] to its leading dimension [[Int]] */
  implicit def unwrap[T](shape: TList[T]): T = shape.head

  /** Short-hand for a [[TList]] with a given size */
  type Of[T, _Size <: Nat] = TList[T] { type Size = _Size }
  type WithPrev[T, _Size <: Nat, _Prev <: Nat] =
    TList[T] {
      type Size = _Size
      type Prev = _Prev
    }

  case class Cons[
    T,
    _P <: Nat
  ](
     head: T,
     _rest: TList.Of[T, _P]
  )(
    implicit
    protected val _size: Succ[_P],
    _toInt: ToInt[Succ[_P]]
  )
  extends TList[T] {
    type Size = Succ[_P]
    type Prev = _P
    //type Tail = TList.Of[T, _P]
    implicit val ev: Size =:= Succ[Prev] = implicitly
    val tail: Some[Tail] = Some(_rest)
    val size = _size.toInt
  }

  case class Base[T](head: T) extends TList[T] {
    type Size = _1
    type Prev = _0
    val _size = _1
    val size = 1
    val tail = None
    implicit val ev: Size =:= Succ[Prev] = implicitly
  }
  object Base {
    implicit def wrap[T](t: T): Base[T] = Base(t)
  }

  import hammerlab.shapeless._

  def apply[T](t: T): Base[T] = Base(t)

  def apply[T](
    s1: T,
    s2: T
  ):
    Cons[T, _1] =
    Cons(
      s1,
      TList(s2)
    )

  def apply[T](
    s1: T,
    s2: T,
    s3: T
  ):
    Cons[
       T,
      _2
    ] =
    Cons(
      s1,
      TList(
        s2,
        s3
      )
    )
}
