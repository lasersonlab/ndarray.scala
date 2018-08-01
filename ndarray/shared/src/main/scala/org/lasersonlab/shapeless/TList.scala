package org.lasersonlab.shapeless

import org.lasersonlab.shapeless.TList.Cons
import shapeless._
import shapeless.Nat._
import shapeless.ops.nat.ToInt

/**
 * Collection of `n` [[Int]]s, with the size stored in the type-level
 *
 * Notable members:
 *
 * - `n`: size of current / "head" dimension
 * - `rest`: "tail" of this type-level list of [[Int]]s, having a size one less than this [[TList]]'s
 * - `size`: number of dimensions ([[Int]]s) in this [[TList]]
 */
sealed abstract class TList[T] {
  def head: T

  type Size <: Nat
  protected implicit def _size: Size
  def size: Int

  type Tail <: Nat
  def tail: Option[TList.Of[T, Tail]]

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
    type Tail = _P
    val tail: Some[TList.Of[T, Tail]] = Some(_rest)
    val size = _size.toInt
  }

  case class Base[T](head: T) extends TList[T] {
    type Size = _1
    type Tail = _0
    val _size = _1
    val size = 1
    val tail = None
  }

  def apply[T](t: T): Base[T] = Base(t)
  def apply[T](s1: T, s2: T): Cons[T, _1] = Cons(s1, TList(s2))(_2, ToInt[_2])
  def apply[T](s1: T, s2: T, s3: T): Cons[T, _2] = Cons(s1, TList(s2, s3))(_3, ToInt[_3])
}
