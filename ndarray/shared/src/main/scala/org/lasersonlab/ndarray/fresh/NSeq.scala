package org.lasersonlab.ndarray.fresh

//import hamm
import shapeless.{ nat, _ }
import nat._
import org.lasersonlab.ndarray.fresh.Array.Idx
import org.lasersonlab.ndarray.fresh.NSeq.Indexed
import org.lasersonlab.ndarray.fresh.Repd.Aux
import org.lasersonlab.ndarray.fresh.TList.{ Base, Cons }

//trait Indexed[T] {
//  def apply(idx: Int): T = ???
//}
//object Indexed {
//  implicit def fromSeq[T](s: Seq[T]): Indexed[T] = ???
//  def apply[T](ts: T*): Indexed[T] = ???
//}
/*
trait Index[S[_]] {
  def apply[T](s: S[T], idx: Int): T
}
trait LowPri {
  implicit val indexedSeq: Index[IndexedSeq] =
    new Index[IndexedSeq] {
      override def apply[T](s: IndexedSeq[T], idx: Int): T = s(idx)
    }
}
object Index extends LowPri {
  implicit val seq: Index[Seq] =
    new Index[Seq] {
      override def apply[T](s: Seq[T], idx: Int): T = s(idx)
    }
}
*/

sealed trait Repd[In] {
  type N <: Nat
  def n: Int
  type Out
  def apply(in: In): Out
}
trait LowPri {
  implicit def zero[T]: Aux[T, _0, T] =
    new Repd[T] {
      type N = _0
      val n = 0
      type Out = T
      def apply(in: T): Out = {
//        println(s"base case: $in")
        in
      }
    }
//  implicit def one[T]: Aux[Seq[T], _1] =
//    new Repd[Seq[T]] {
//      type N = _1
//      val n = 1
//    }
}
object Repd
  extends LowPri {
  type Aux[In, _N <: Nat, _Out] = Repd[In] { type N = _N; type Out = _Out }
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
      def apply(in: R): Out = {
//        println(s"converting range: $in")
        in
      }
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
      def apply(in: I[PrevIn]): Out = {
        val out = in.toSeq.map(r.value(_))
//        println(s"level $n: $in $out")
        out
      }
    }
}

object Foo {
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

trait NSeq[N <: Nat] {
  type Out[T]
  def apply[T](idx: Idx[N], seqs: Out[T]): T
}

object NSeq {
  type Indexed[+T] = Seq[T]
  val Indexed = Seq

  type Aux[N <: Nat, _Out[_]] = NSeq[N] { type Out[T] = _Out[T] }

  implicit val bases: NSeq.Aux[_1, Indexed] =
    new NSeq[_1] {
      type Out[T] = Indexed[T]
      def apply[T](idx: Idx[nat._1], seqs: Out[T]): T = seqs(idx.head)
    }

  implicit def succ[N <: Nat](implicit n: NSeq[N]): NSeq.Aux[Succ[N], λ[T ⇒ Indexed[n.Out[T]]]] =
    new NSeq[Succ[N]] {
      type Out[T] = Indexed[n.Out[T]]
      def apply[T](
        idx: Idx[Succ[N]],
        seqs: Out[T]
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

  val one = the[NSeq.Aux[_1, Indexed]]
  the[NSeq[_1]]
  the[NSeq.Aux[_1, λ[T ⇒ Indexed[T]]]]
//  val one = bases[Seq]
  one(TList.Base(1), Seq(1, 2, 3))
  val x: one.Out[Int] = Seq(1, 2, 3)

  val two = succ[_1]
  two(TList.Cons(0, Base(1)), Indexed(Indexed(1, 2, 3)))
  val y: two.Out[Int] = Indexed(Indexed(1, 2, 3))

  val two2 = the[NSeq.Aux[_2, λ[T ⇒ Indexed[Indexed[T]]]]]
  two2(TList.Cons(0, Base(1)), Indexed(Indexed(1, 2, 3)))
  val y2: two2.Out[Int] = Indexed(Indexed(1, 2, 3))

  val two3 = the[NSeq[_2]]
  two3(TList.Cons(0, Base(1)), Indexed(Indexed(1, 2, 3)))
  val y3: two3.Out[Int] = Indexed(Indexed(1, 2, 3))

  succ[_2]

  val three = the[NSeq[_3]]
  val z: three.Out[Int] = Indexed(Indexed(Indexed(1, 2, 3)))

  val three3 = the[NSeq.Aux[_3, λ[T ⇒ Indexed[Indexed[Indexed[T]]]]]]
  val z3: three3.Out[Int] = Indexed(Indexed(Indexed(1, 2, 3)))

}

trait Array[T, N <: Nat] {
  def apply(idx: Idx[N]): T
}
object Array {
  type Idx[N <: Nat] = TList[Int, N]
  //val Idx = TList.apply()[Int] _

  sealed case class Arg[Out](out: Out)
  object Arg {
    implicit def make[In, N <: Nat, Out](in: In)(implicit r: Repd.Aux[In, N, Out]): Arg[Out] = Arg(r(in))
  }

  def apply[
    T,
    N <: Nat,
    S[U]
  ](
    args: Arg[S[T]]*
  )(
    implicit
    nseq: NSeq.Aux[Succ[N], λ[A ⇒ Seq[S[A]]]]
  ):
    Array[T, Succ[N]] =
    Seqs[
      T,
      Succ[N],
      λ[T ⇒ Seq[S[T]]]
    ](
      args.map(_.out)
    )(
      nseq
    )

  case class Seqs[
    T,
    N <: Nat,
    S[_] <: Indexed[_]
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
