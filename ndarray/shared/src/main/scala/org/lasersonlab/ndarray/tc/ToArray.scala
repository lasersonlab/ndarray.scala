package org.lasersonlab.ndarray.tc

import hammerlab.shapeless._
import org.lasersonlab.ndarray.Array.Idx
import shapeless.{ Nat, Succ }
import hammerlab.shapeless
import org.lasersonlab.ndarray.Array

trait ToArray[T, N <: Nat] {
  type Elem
  def apply(t: T): Array.Aux[Elem, N]
}

trait LowPriToArray {

  type Aux[T, N <: Nat, _Elem] = ToArray[T, N] { type Elem = _Elem }

  def apply[T, N <: Nat, _Elem](fn: T ⇒ Array.Aux[_Elem, N]): Aux[T, N, _Elem] =
    new ToArray[T, N] {
      type Elem = _Elem
      @inline def apply(t: T): Array.Aux[_Elem, N] = fn(t)
    }

  implicit def singleton[T]: Aux[T, _0, T] =
    apply {
      t ⇒
        new Array[T] {
          type N = _0
          val n: Int = 0
          val shape: Idx[shapeless._0] = ???
          def apply(idx: Idx[shapeless._0]): T = ???
        }
    }
}

object ToArray
  extends LowPriToArray {
  implicit def seq[T, _N <: Nat, Elem](implicit prev: Aux[T, _N, Elem]): Aux[Seq[T], Succ[_N], Elem] =
    apply {
      t ⇒
        val arrs = t.map(prev(_))
        val head = arrs.head
        arrs
          .tail
          .forall {
            arr ⇒
              if (head.shape != arr.shape)
                throw new IllegalArgumentException(
                  s"Mismatched shapes: ${head.shape}, ${arr.shape}"
                )
          }

    }
}
