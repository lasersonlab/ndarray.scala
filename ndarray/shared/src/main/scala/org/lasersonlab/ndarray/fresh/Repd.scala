package org.lasersonlab.ndarray.fresh

import shapeless._, nat._

sealed trait Repd[In] {
  type N <: Nat
  def n: Int
  type Out
  def apply(in: In): Out
}

trait LowPri {
  type Aux[In, _N <: Nat, _Out] =
    Repd[In] {
      type N = _N
      type Out = _Out
    }

  implicit def zero[T]: Aux[T, _0, T] =
    new Repd[T] {
      type N = _0
      val n = 0
      type Out = T
      def apply(in: T): Out = in
    }
}

object Repd
  extends LowPri {
  implicit def range[R <: Range]:
    Aux[
      R,
      _1,
      Seq[Int]
    ] =
    new Repd[R] {
      type N = _1
      val n = 1
      type Out = Seq[Int]
      def apply(in: R): Out = in
    }

  implicit def cons[
    _N <: Nat,
    I[T] <: Seq[T],
    PrevIn,
    PrevOut
  ](
    implicit
    r: Lazy[Aux[PrevIn, _N, PrevOut]]
  ):
    Aux[
      I[PrevIn],
      Succ[_N],
      Seq[PrevOut]
    ] =
    new Repd[I[PrevIn]] {
      type N = Succ[_N]
      val n = r.value.n + 1
      type Out = Seq[r.value.Out]
      def apply(in: I[PrevIn]): Out =
        in.map(r.value(_))
    }
}
