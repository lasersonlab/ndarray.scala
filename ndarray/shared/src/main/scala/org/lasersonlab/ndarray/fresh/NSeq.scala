package org.lasersonlab.ndarray.fresh

import shapeless._
import nat._
import org.lasersonlab.ndarray.fresh.Array.Idx
import org.lasersonlab.ndarray.fresh.TList.{ Base, Cons }

/**
 * Given an [[N]]-element [[Idx index]] structure ([[N]] integer coordinates), index into an [[N]]-nested [[Seq]] to
 * pull out a single element of type [[T]]
 *
 * @tparam T element type to return
 * @tparam N number of dimensions in index and [[N]]-d [[Seq]]-array
 */
trait NSeq[T, N <: Nat] {
  type Out
  def apply(idx: Idx[N], seqs: Out): T
}

object NSeq {

  type Aux[T, N <: Nat, _Out] = NSeq[T, N] { type Out = _Out }

  /**
   * Base-case: [[_1 1]]-D flat [[Seq]]
   */
  implicit def base[T]: NSeq.Aux[T, _1, Seq[T]] =
    new NSeq[T, _1] {
      type Out = Seq[T]
      def apply(idx: Idx[nat._1], seqs: Out): T = seqs(idx.head)
    }

  /**
   * Inductive step: [[N]]+1 dimensions implemented in terms of [[N]] dimensions
   * @param n [[N]]-dimensional indexer pulling out an element of type [[T]]
   * @tparam T element type returned [[Idx]]-application
   * @tparam N previous number of dimensions
   * @return indexer for structures that are one level of [[Seq]] more deeply nested than in `n`
   */
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
}
