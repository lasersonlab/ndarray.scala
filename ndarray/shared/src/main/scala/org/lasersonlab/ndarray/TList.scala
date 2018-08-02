package org.lasersonlab.ndarray

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

  implicit def wrap[T, Elem, N <: Nat](t: T)(implicit ev: IsTList[T, Elem, N]): TList[Elem, N] = ev(t)
}
