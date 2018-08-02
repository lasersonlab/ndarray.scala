package org.lasersonlab.ndarray

import shapeless._, nat._

sealed trait Unroll[In] {
  type N <: Nat
  type Out
}

trait LowPriUnroll {
  type Aux[In, _N <: Nat, _Out] =
    Unroll[In] {
      type N = _N
      type Out = _Out
    }

  type Natd[In, _N <: Nat] =
    Unroll[In] {
      type N = _N
    }

  implicit def zero[T]: Aux[T, _0, T] =
    new Unroll[T] {
      type N = _0
      type Out = T
    }
}

object Unroll
  extends LowPriUnroll {
  implicit def rep[
    In,
    _N <: Nat,
    _Out
  ](
    implicit
    prev: Lazy[Aux[In, _N, _Out]]
  ):
    Aux[
      Seq[In],
      Succ[_N],
      _Out
    ] =
    new Unroll[Seq[In]] {
      type N = Succ[_N]
      type Out = _Out
    }
}
