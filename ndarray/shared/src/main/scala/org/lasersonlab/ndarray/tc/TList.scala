package org.lasersonlab.ndarray.tc

import shapeless.Nat
import shapeless.nat._

//sealed trait TList[T] {
//  type N <: Nat
//  type P <: Nat
//  def head: T
//  def tail: TList[T]
//}
//
//object TList {
//  type Aux[T, _N, _P] =
//    TList[T] {
//      type N <: _N
//      type P <: _P
//    }
//
//  implicit def empty[T]: Aux[T, _0, Nothing] =
//    new TList[T] {
//      override type N = _0
//      override type P = Nothing
//      override def head: T = ???
//      override def tail: TList[T] = ???
//    }
//}
