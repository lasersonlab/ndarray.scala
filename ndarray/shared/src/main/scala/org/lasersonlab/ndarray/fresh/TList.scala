package org.lasersonlab.ndarray.fresh

import shapeless._, nat._

sealed trait TList[T, N <: Nat] {
  def head: T
}
object TList {
  case class Base[T](head: T) extends TList[T, _1]
  object Base {
    implicit def wrap[T](t: T): Base[T] = Base(t)
  }
  case class Cons[T, P <: Nat](head: T, tail: TList[T, P]) extends TList[T, Succ[P]]
  def apply[T](t: T): Base[T] = Base(t)
  def apply[T](t1: T, t2: T): Cons[T, _1] = Cons(t1, Base(t2))
  def apply[T](t1: T, t2: T, t3: T): Cons[T, _2] = Cons(t1, Cons(t2, Base(t3)))
  def apply[T](t1: T, t2: T, t3: T, t4: T): Cons[T, _3] = Cons(t1, Cons(t2, Cons(t3, Base(t4))))
}
