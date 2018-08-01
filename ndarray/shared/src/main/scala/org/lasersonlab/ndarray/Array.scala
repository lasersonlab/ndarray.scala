package org.lasersonlab.ndarray

import org.lasersonlab.ndarray.Array.Idx
import org.lasersonlab.shapeless.TList
import shapeless.{ Nat, Succ }
import shapeless.Nat._
import shapeless.ops.nat.ToInt

import scala.reflect.ClassTag

trait Array[T] {
  // Number of dimensions
  type N <: Nat

  // N-element list of [[Int]] indices
  type Shape <: Idx[N]
  def shape: Shape

  def apply(shape: Shape): T
}

object Array {

  //implicit def unwrap[T](array: Array[T]): scala.Array[array.Reduced] = array.data

  type Idx[N <: Nat] = TList.Of[Int, N]

  def apply[T](elems: T*): Base[T] = Base(elems)

  implicit def lift[
    T,
    P <: Nat,
    _Shape <: Idx[P]
  ](
    ts: Seq[Array.Of[T, P, _Shape]]
  )(
    implicit
    n: Succ[P],
    toInt: ToInt[Succ[P]],
    pShape: _Shape
  ):
    Array.Of[T, Succ[P], TList.Cons[Int, P, _Shape]] =
    Rec[
      T, P, _Shape
    ](
      ts
    )(
      n,
      pShape,
      toInt
    )

  case class Base[T](data: Seq[T])
    extends Array[T] {
    type N = _1
    type Shape = TList.Base[Int]
    val shape: Shape = TList(data.length)
    def apply(shape: Shape): T = data(shape.head)
  }
  object Base {
    implicit def wrap[T](ts: Seq[T]): Base[T] = Base(ts)
  }

  type Of[T, _N <: Nat, _Shape <: Idx[_N]] =
    Array[T] {
      type N = _N
      type Shape = _Shape
    }

  case class Rec[
    T,
    P <: Nat,
    _Shape <: Idx[P]
  ](
    data: Seq[Array.Of[T, P, _Shape]]
  )(
    implicit
    val n: Succ[P],
    reducedShape: _Shape,
    toInt: ToInt[Succ[P]]
  )
  extends Array[T] {
    type N = Succ[P]
    type Shape = TList.Cons[Int, P, _Shape]
    override def shape: Shape = reducedShape.prepend(data.length)
    def apply(shape: Shape): T = data(shape.head)(shape.tail.get)
  }
}
