package org.lasersonlab.ndarray.fresh

import shapeless._
import nat._
import org.lasersonlab.ndarray.fresh.Array.Idx
import org.lasersonlab.ndarray.fresh.TList.{ Base, Cons }
import shapeless.ops.nat.ToInt

sealed trait Unroll[In] {
  type N <: Nat
  def n: Int
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
      val n = 0
      type Out = T
    }
}
object Unroll
  extends LowPriUnroll {
  implicit def rep[In, _N <: Nat, _Out](implicit prev: Lazy[Aux[In, _N, _Out]]): Aux[Seq[In], Succ[_N], _Out] =
    new Unroll[Seq[In]] {
      type N = Succ[_N]
      def n: Int = prev.value.n + 1
      type Out = _Out
    }
}

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

object Foo {
  {
    import Repd.Aux
    implicitly[Repd[Seq[Int]]]
    implicitly[Aux[Seq[Int], _1, Seq[Int]]]
    implicitly[Aux[Vector[Int], _1, Seq[Int]]]
    implicitly[Aux[Range, _1, Seq[Int]]]
    implicitly[Aux[Range.Inclusive, _1, Seq[Int]]]
    implicitly[Aux[Range, _1, Seq[Int]]]
    implicitly[Repd[Range.Inclusive]]
    implicitly[Repd[Range]]

    implicitly[Repd[Seq[Seq[Int]]]]
    implicitly[Aux[Seq[Seq[Int]], _2, Seq[Seq[Int]]]]
    implicitly[Aux[Vector[IndexedSeq[Int]], _2, Seq[Seq[Int]]]]

    implicitly[Repd[Seq[Seq[Seq[Int]]]]]
    implicitly[Aux[Seq[Seq[Seq[Int]]], _3, Seq[Seq[Seq[Int]]]]]
  }
  {
    import Unroll.Aux

    the[Unroll[Seq[Int]]]
    the[Unroll[Seq[Seq[Int]]]]
    the[Unroll[Seq[Seq[Seq[Int]]]]]

    the[Aux[Int, _0, Int]]
    the[Aux[Seq[Int], _1, Int]]
    the[Aux[Seq[Seq[Int]], _2, Int]]
    the[Aux[Seq[Seq[Seq[Int]]], _3, Int]]
  }
}

trait NSeq[T, N <: Nat] {
  type Out
  def apply(idx: Idx[N], seqs: Out): T
}

object NSeq {

  type Aux[T, N <: Nat, _Out] = NSeq[T, N] { type Out = _Out }

  implicit def bases[T]: NSeq.Aux[T, _1, Seq[T]] =
    new NSeq[T, _1] {
      type Out = Seq[T]
      def apply(idx: Idx[nat._1], seqs: Out): T = seqs(idx.head)
    }

  implicit def succ[T, N <: Nat](implicit n: NSeq[T, N]): NSeq.Aux[T, Succ[N], Seq[n.Out]] =
    new NSeq[T, Succ[N]] {
      type Out = Seq[n.Out]
      def apply(
        idx: Idx[Succ[N]],
        seqs: Out
      ):
        T =
        idx match {
          case Cons(head, tail) ⇒
            n.apply(
              tail,
              seqs(head)
            )
          case Base(head) ⇒ ???
        }
    }

  val one = the[NSeq.Aux[Int, _1, Seq[Int]]]
  the[NSeq[Int, _1]]
  the[NSeq.Aux[Int, _1, Seq[Int]]]
//  val one = bases[Seq]
  one(TList.Base(1), Seq(1, 2, 3))
  val x: one.Out = Seq(1, 2, 3)

  val two = succ[Int, _1]
  two(TList.Cons(0, Base(1)), Seq(Seq(1, 2, 3)))
  val y: two.Out = Seq(Seq(1, 2, 3))

  val two2 = the[NSeq.Aux[Int, _2, Seq[Seq[Int]]]]
  two2(TList.Cons(0, Base(1)), Seq(Seq(1, 2, 3)))
  val y2: two2.Out = Seq(Seq(1, 2, 3))

  val two3 = the[NSeq[Int, _2]]
  two3(TList.Cons(0, Base(1)), Seq(Seq(1, 2, 3)))
  val y3: two3.Out = Seq(Seq(1, 2, 3))

  succ[Int, _2]

  val three = the[NSeq[Int, _3]]
  val z: three.Out = Seq(Seq(Seq(1, 2, 3)))

  val three3 = the[NSeq.Aux[Int, _3, Seq[Seq[Seq[Int]]]]]
  val z3: three3.Out = Seq(Seq(Seq(1, 2, 3)))

}

trait Array[T, N <: Nat] {
  def n: N
  def apply(idx: Idx[N]): T
}
object Array {
  type Idx[N <: Nat] = TList[Int, N]

  sealed case class Arg[Out, N <: Nat](out: Out, n: Int)
  object Arg {
    implicit def make[
      In,
      N <: Nat,
      Out
    ](
      in: In
    )(
      implicit
      r: Repd.Aux[In, N, Out],
      n: N,
      toInt: ToInt[N]
    ):
      Arg[
        Out,
        N
      ] =
      Arg(
        r(in),
        toInt()
      )
  }

  def apply[
    T,
    N <: Nat,
    Nxt <: Succ[N],
    S
  ](
    args: Arg[S, N]*
  )(
    implicit
    unroll: Unroll.Aux[Seq[S], Nxt, T],
    nseq: NSeq.Aux[T, Nxt, Seq[S]],
    n: Nxt,
    ti: ToInt[N],
    toInt: ToInt[Nxt]
  ):
    Array[T, Nxt] = {
    println(s"arg degrees: ${args.map(_.n).mkString(",")}… ${ti()} ${toInt()}")
    Seqs[
      T,
      Nxt,
      S
    ](
      args.map(_.out)
    )(
      nseq,
      n,
      toInt
    )
  }

  case class Seqs[
    T,
    N <: Nat,
    S
  ](
    v: Seq[S]
  )(
    implicit
    nseq: NSeq.Aux[T, N, Seq[S]],
    val n: N,
    toInt: ToInt[N]
  )
  extends Array[T, N]
  {
    println(s"made array: ${toInt()}")
    def apply(idx: Idx[N]): T = nseq(idx, v)
  }
}
