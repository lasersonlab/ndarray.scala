package org.lasersonlab.ndarray

import Array.Idx
import shapeless._
import shapeless.ops.nat.ToInt

/**
 * N-dimensional array interface
 *
 * @tparam T element type
 */
trait Array[T] {
  type N <: Nat
  def n: Int
  def apply(idx: Idx[N]): T
}

object Array {

  type Aux[T, _N <: Nat] = Array[T] { type N = _N }

  /**
   * Index into an [[N]]-dimensional [[Array]]: a [[TList typed list]] with [[N]] [[Int integers]]s
   */
  type Idx[N <: Nat] = TList[Int, N]

  /**
   * Wrapper type for [[Array]]-construction DSL
   *
   * For various types that are isomorphic to [[N]]-dimensional [[Seq]]s (e.g. [[N]]-level nests of more specialized
   * [[Seq]] implementations like [[IndexedSeq]], [[Vector]], etc.), homogenize all layers to [[Seq]] to get simplify
   * type-calculations in [[apply Array-construction]] below
   *
   * @param out converted/wrapped [[N]]-dimensional [[Seq]]
   * @tparam Out [[N]]-nested [[Seq]] structure
   * @tparam N number of levels of [[Seq]]-nesting
   */
  sealed case class Arg[Out, N <: Nat](out: Out)
  object Arg {
    /**
     * If an [[In input type]] can be represented as [[N]] levels of [[Seq]] (cf. [[Repd]] evidence `r` below), perform
     * that conversion, and wrap the output in an [[Arg]] instance.
     *
     * @param in input instance; [[N]]-nested [[Seq]]-like structure (each level may be a more specific [[Seq]]-subtype)
     * @param r evidence that [[In]] is equivalent to an [[N]]-nested [[Seq]]
     * @tparam In input type; [[N]]-nested [[Seq]]-like structure (each level may be a more specific [[Seq]]-subtype)
     * @tparam N number of levels of nestedness in [[In]] and [[Out]]
     * @tparam Out output type: [[N]]-nested [[Seq]]
     * @return input `in` converted to output type [[Out]], wrapped in an [[Arg]], ready for use in
     *         [[apply Arry-construction]]
     */
    implicit def make[
      In,
      N <: Nat,
      Out
    ](
      in: In
    )(
      implicit
      r: Repd.Aux[In, N, Out]
    ):
      Arg[
        Out,
        N
      ] =
      Arg(
        r(in)
      )
  }

  /**
   * Create an n-dimensional array from arguments that are (or can be converted to) [[N]] nested layers of [[Seq]]s
   * wrapping elements of type [[T]]
   *
   * @param args wrap raw elements in [[Arg]]s, which has the effect of homogenizing various allowed structures that are
   *             all isomorphic to nested [[Seq]]s
   * @param unroll evidence that the sequence of `S`s (wrapped in `args` above) represents [[N]] levels of nested
   *               [[Seq]]s around elements of type [[T]]
   * @param nseq implements indexing into an [[N]]-dimensional [[Array]] by an [[N]]-dimensional [[Idx]]
   * @tparam T type of elements stored in this [[Array]]
   * @tparam ArgN dimension of the individual arguments to this function (wrapped in [[Arg]]s); the dimension [[N]] of
   *              the resulting array is one more than this (the varargs prepends a dimension)
   * @tparam N type-level number of dimensions in this [[Array]]
   * @tparam S the type wrapped in each [[Arg argument]]: elements of type [[T]] nested in [[ArgN]] levels (one less
   *           than [[N]]) of [[Seq]]
   * @return [[N]]-d [[Array]] wrapping the input arguments
   */
  def apply[
    T,
    ArgN <: Nat,
    N <: Succ[ArgN],
    S
  ](
    args: Arg[S, ArgN]*
  )(
    implicit
    unroll: Unroll.Aux[Seq[S], N, T],
    nseq: NSeq.Aux[T, N, Seq[S]],
    toInt: ToInt[N]
  ):
    Array.Aux[T, N] =
    Seqs[
      T,
      N,
      Seq[S]
    ](
      args.map(_.out)
    )(
      nseq,
      toInt
    )

  /**
   * [[_N]]-dimensional [[Array]] implementation wrapping an [[_N]]-nested [[Seq]] data-structure
   *
   * @param data wrapped [[_N]]-D data: an [[_N]]-level [[Seq]] of [[T]]s
   * @param nseq implementation of [[_N]]-dimensional indexing, taking an [[_N]]-D [[Idx index]], applying it to this
   *             [[Array]], and returning a single [[T element]]
   * @param toInt convert type-level number of dimensions [[_N]] into an [[Int]], for runtime/value-level access
   * @tparam T element type
   * @tparam _N (type-level) number of dimensions
   * @tparam Data [[_N]]-level [[Seq]] of [[T]]s
   */
  case class Seqs[
    T,
    _N <: Nat,
    Data
  ](
     data: Data
  )(
    implicit
    nseq: NSeq.Aux[T, _N, Data],
    toInt: ToInt[_N]
  )
  extends Array[T]
  {
    type N = _N
    val n = toInt()
    @inline def apply(idx: Idx[_N]): T = nseq(idx, data)
  }

  case class Fn[
    T,
    _N <: Nat
  ](
    fn: Idx[_N] ⇒ T
  )(
    implicit
    toInt: ToInt[_N]
  )
  extends Array[T]
  {
    type N = _N
    val n: Int = toInt()
    @inline def apply(idx: Idx[_N]): T = fn(idx)
  }
}
