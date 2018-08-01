package org.lasersonlab.ndarray

import org.lasersonlab.shapeless.TList
import shapeless.{ Nat, Succ }
import shapeless.Nat._
import shapeless.ops.nat.ToInt

import scala.reflect.ClassTag

trait Array[T] {
  // Number of dimensions
  type N <: Nat

  type Shape <: TList.Of[Int, N]
  def shape: Shape

  def apply(shape: Shape): T

  // (N-1)-dimensional arrays
  type Reduced
  //def data: scala.Array[Reduced]
}

object Array {

  //implicit def unwrap[T](array: Array[T]): scala.Array[array.Reduced] = array.data

  type Idx[N <: Nat] = TList.Of[Int, N]

  def apply[T](elems: T*): Base[T] = Base(elems)

  implicit def lift[
    T,
    P <: Nat,
    A <: Array.Of[T, P, Idx[P]]
  ](
    ts: Seq[A]
  )(
    implicit
    n: Succ[P],
    toInt: ToInt[Succ[P]],
    pShape: Idx[P]
  ):
    Array.Of[T, Succ[P], TList.Cons[Int, P]] =
    Rec[
      T, P, A
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
    type Reduced = T
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
    Red <: Array.Of[T, P, Idx[P]]
  ](
    data: Seq[Red]
  )(
    implicit
    val n: Succ[P],
    reducedShape: Idx[P],
    toInt: ToInt[Succ[P]]
  )
  extends Array[T] {
    type N = Succ[P]
    type Shape = TList.Cons[Int, P]
    override def shape: Shape = reducedShape.prepend(data.length)
    override type Reduced = Red
    def apply(shape: Shape): T = data(shape.head)(shape.tail.get)
  }
}
