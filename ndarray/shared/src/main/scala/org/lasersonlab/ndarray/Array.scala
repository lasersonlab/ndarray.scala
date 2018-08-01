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
  type Shape = Idx[N]
  def shape: Shape

  type P <: Nat
  implicit def ev: N =:= Succ[P]

  def apply(shape: TList.WithPrev[Int, N, P]): T
}

object Array {

  //implicit def unwrap[T](array: Array[T]): scala.Array[array.Reduced] = array.data

  type Idx[N <: Nat] = TList.Of[Int, N]

  def apply[
    T,
    P <: Nat,
    A <: Array.Of[T, P]
  ](
    elems: A*
  )(
    implicit
    n: Succ[P],
    toInt: ToInt[Succ[P]]
  ):
  Array.Of[
    T,
    Succ[P]
  ] =
    lift[T, P](elems)

  implicit def lift[
    T,
    P <: Nat
  ](
    ts: Seq[Array.Of[T, P]]
  )(
    implicit
    n: Succ[P],
    toInt: ToInt[Succ[P]]
  ):
    Array.Of[
      T,
      Succ[P]
    ] =
    Rec[
      T, P
    ](
      ts
    )(
      n,
      toInt
    )

  case class Base[T](data: Seq[T])
    extends Array[T] {
    type N = _1

    type P = _0
    implicit val ev: N =:= Succ[P] = implicitly

    //type Shape = TList.Base[Int]
    val shape: Shape = TList(data.length)
    def apply(shape: TList.WithPrev[Int, N, P]): T = data(shape.head)
  }
  object Base {
    implicit def wrap[T](ts: Seq[T]): Base[T] = Base(ts)
  }

  type Of[T, _N <: Nat] =
    Array[T] {
      type N = _N
      type Shape <: Idx[N]
    }

  case class Rec[
    T,
    _P <: Nat
  ](
    data: Seq[Array.Of[T, _P]]
  )(
    implicit
    val n: Succ[_P],
    toInt: ToInt[Succ[_P]]
  )
  extends Array[T] {
    type N = Succ[P]
    type P = _P
    implicit val ev: N =:= Succ[P] = implicitly
    //override type Shape = TList.Cons[Int, P]

    override val shape: TList.Cons[Int, P] = data.head.shape.prepend(data.length)

    implicitly[shape.Prev =:= P]
    def apply(shape: TList.WithPrev[Int, N, P]): T = {
      ???
//      import shape.ev
//      implicitly[shape.Size =:= N]
//      implicitly[shape.Prev =:= P]
//      implicitly[shape.Prev =:= _P]
//      data(shape.head)(shape.tail.get)
    }
  }
}
