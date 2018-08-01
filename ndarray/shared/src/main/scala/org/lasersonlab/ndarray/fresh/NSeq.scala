package org.lasersonlab.ndarray.fresh

import shapeless.{ nat, _ }
import nat._
import org.lasersonlab.ndarray.fresh.Array.Idx
import org.lasersonlab.ndarray.fresh.TList.{ Base, Cons }

trait NSeq[N <: Nat] {
  type Out[_]
  def apply[T](idx: Idx[N], seqs: Out[T]): T
}
object NSeq {
  type Aux[N <: Nat, _Out[_] <: Seq[_]] = NSeq[N] { type Out[_] = _Out[_] }

  implicit val base =
    new NSeq[_1] {
      type Out[T] = Seq[T]
      def apply[T](idx: Idx[nat._1], seqs: Seq[T]): T = ???
    }

  implicit def succ[N <: Nat](implicit n: NSeq[N]): NSeq[Succ[N]] { type Out[A] = Seq[n.Out[A]] } =
    new NSeq[Succ[N]] {
      type Out[A] = Seq[n.Out[A]]
      def apply[T](
        idx: Idx[Succ[N]],
        seqs: Seq[n.Out[T]]
      ):
        T =
        idx match {
          case Cons(head, tail) ⇒
            n(
              tail,
              seqs(head)
            )
          case Base(head) ⇒ ???
        }
    }

  val one = the[NSeq[_1]]
  val x: one.Out[Int] = Seq(1, 2, 3)

  val two = the[NSeq[_2]]
  val y: two.Out[Int] = Seq(Seq(1, 2, 3))

  val three = the[NSeq[_3]]
  val z: three.Out[Int] = Seq(Seq(Seq(1, 2, 3)))
}

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

trait Array[T, N <: Nat] {
  def apply(idx: Idx[N]): T
}
object Array {
  type Idx[N <: Nat] = TList[Int, N]

//  def apply[T, N <: Nat, S[_] <: Seq[_]](s: S[T])(implicit nseq: NSeq.Aux[N, S]): Array[T, N] = Seqs(s)

  def apply[
    T,
    P <: Nat,
    S[_] <: Seq[_]
  ](
    s: S[T]*
  )(
    implicit
    nseq: NSeq.Aux[Succ[P], λ[T ⇒ Seq[S[T]]]]
  ):
    Array[T, Succ[P]] =
    Seqs[T, Succ[P], λ[T ⇒ Seq[S[T]]]](s)(nseq)

  case class Seqs[
    T,
    N <: Nat,
    S[_] <: Seq[_]
  ](
    v: S[T]
  )(
    implicit
    nseq: NSeq.Aux[N, S]
  )
  extends Array[T, N]
  {
    def apply(idx: Idx[N]): T = nseq(idx, v)
  }
}
