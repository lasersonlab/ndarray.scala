package org.lasersonlab.ndarray

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
sealed abstract class TList {
  def head: Int

  type Size <: Nat
  protected implicit def _size: Size
  def size: Int

  type Tail <: Nat
  def tail: Option[TList.Of[Tail]]
}
object TList {

  /** Can implicitly convert a [[TList]] to its leading dimension [[Int]] */
  implicit def unwrap(shape: TList): Int = shape.head

  /** Short-hand for a [[TList]] with a given size */
  type Of[_Size <: Nat] = TList { type Size = _Size }

  case class Tail[
    _P <: Nat
  ](
     head: Int,
     _rest: TList.Of[_P]
  )(
     implicit
     protected val _size: Succ[_P],
     _toInt: ToInt[Succ[_P]]
  )
  extends TList {
    type Size = Succ[_P]
    type Tail = _P
    val tail: Some[TList.Of[Tail]] = Some(_rest)
    val size = _size.toInt
  }

  case class Base(head: Int) extends TList {
    type Size = _1
    type Tail = _0
    val _size = _1
    val size = 1
    val tail = None
  }

  def apply(n: Int): Base = Base(n)
  def apply(s1: Int, s2: Int): Tail[_1] = Tail(s1, TList(s2))(_2, ToInt[_2])
  def apply(s1: Int, s2: Int, s3: Int): Tail[_2] = Tail(s1, TList(s2, s3))(_3, ToInt[_3])
}
