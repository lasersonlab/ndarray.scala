package org.lasersonlab.ndarray.tc

import org.hammerlab.shapeless.tlist.TList.{ Base, Cons }
import shapeless.ops.nat.Pred
import shapeless.{ Nat, Succ }

//trait Array[T] {
//  type N <: Nat
//  def shape: Idx[N]
////  def apply[P <: Nat](idx: Int)(implicit ev: Pred.Aux[N, P]): Array.Aux[T, P]
//  def apply(idx: Idx[N]): T
//}
//
//object Array {
//  type Aux[T, _N <: Nat] = Array[T] { type N = _N }
//
//  def apply[T, _N <: Nat](arrs: Array.Aux[T, _N]*): Array.Aux[T, Succ[_N]] = {
//    val _shape =
//      arrs
//        .map(_.shape)
//        .toList match {
//        case h :: t ⇒
//          t
//            .find(_ != h)
//            .foreach {
//              t ⇒
//                throw new IllegalArgumentException(
//                  s"Mismatched shapes: $h, $t"
//                )
//            }
//          h
//        case _ ⇒
//          throw new IllegalArgumentException(
//            s"Can't concatenate an empty list of arrays"
//          )
//      }
//    new Array[T] {
//      type N = Succ[_N]
//      val shape = Cons(arrs.size, _shape)
//      def apply(idx: Idx[Succ[_N]]): T =
//        idx match {
////          case Base(h) ⇒
////            arrs(h)
//          case Cons(h, rest) ⇒
//            arrs(h)(rest)
//        }
//    }
//  }
//}
