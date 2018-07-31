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
 * - `rest`: "tail" of this type-level list of [[Int]]s, having a size one less than this [[Shape]]'s
 * - `size`: number of dimensions ([[Int]]s) in this [[Shape]]
 */
sealed abstract class Shape {
  def n: Int

  type Size <: Nat
  protected implicit def _size: Size
  def size: Int

  type Rest <: Nat
  def rest: Option[Shape.Of[Rest]]
}
object Shape {

  /** Can implicitly convert a [[Shape]] to its leading dimension [[Int]] */
  implicit def unwrap(shape: Shape): Int = shape.n

  /** Short-hand for a [[Shape]] with a given size */
  type Of[_Size <: Nat] = Shape { type Size = _Size }

  case class Tail[
    _P <: Nat
  ](
    n: Int,
    _rest: Shape.Of[_P]
  )(
     implicit
     protected val _size: Succ[_P],
     _toInt: ToInt[Succ[_P]]
  )
  extends Shape {
    type Size = Succ[_P]
    type Rest = _P
    val rest: Some[Shape.Of[Rest]] = Some(_rest)
    val size = _size.toInt
  }

  case class Base(n: Int) extends Shape {
    type Size = _1
    type Rest = _0
    val _size = _1
    val size = 1
    val rest = None
  }

  def apply(n: Int): Base = Base(n)
  def apply(s1: Int, s2: Int): Tail[_1] = Tail(s1, Shape(s2))(_2, ToInt[_2])
  def apply(s1: Int, s2: Int, s3: Int): Tail[_2] = Tail(s1, Shape(s2, s3))(_3, ToInt[_3])
}

//sealed trait Array[N <: Nat, T] {
//  def apply(shape: Shape[N]): T
//}
//
//case class `1D`(value: scala.Array[T]) extends Array[_1, T]
//
//object Array {
//
//}
