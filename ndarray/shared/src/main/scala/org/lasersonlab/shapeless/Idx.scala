package org.lasersonlab.shapeless

import shapeless._, nat._

trait Idx[T, Size <: Nat] {
  def head: T
  //def tail[P <: Nat](implicit p: P, ev: =:=[Size, Succ[P]]): Idx[T, P]
}
//object Idx {
//  implicit def base[T](t: T): Idx[T, _1] =
//    new Idx[T, _1] {
//      val head = t
//    }
//
//  implicit def cons[T, P <: Nat](t: T)()
//}
