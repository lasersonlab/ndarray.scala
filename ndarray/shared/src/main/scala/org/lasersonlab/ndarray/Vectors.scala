package org.lasersonlab.ndarray

import cats.implicits._
import cats.{ Applicative, Eval, Id, Traverse }
import shapeless.Lazy

trait Vectors[T] {
  type Row[U]
  def rows: Vector[Row[T]]
  def size = rows.length
  implicit def traverseRow: Traverse[Row]
  def apply(idx: Int): Row[T] = rows(idx)
}

object Vectors {
  type Aux[T, _R[_]] = Vectors[T] { type Row[U] = _R[U] }

  /**
   * Convenience constructor for [[Vectors]] instances
   */
  def make[
    T,
    _Row[U]
  ](
    _rows: Vector[_Row[T]]
  )(
    implicit
    _traverseRow: Traverse[_Row]
  ):
    Aux[T, _Row] =
    new Vectors[T] {
      type Row[U] = _Row[U]
      val rows = _rows
      override val size = rows.size
      val traverseRow = _traverseRow
    }

  implicit val traverse: Traverse[Vectors] =
    new Traverse[Vectors] {
      override def traverse[
        G[_],
        A,
        B
      ](
        fa: Vectors[A]
      )(
        f: A ⇒ G[B]
      )(
        implicit
        ev: Applicative[G]
      ):
        G[Vectors[B]] = {
        implicit val tr = fa.traverseRow
        fa
          .rows
          .map {
            row ⇒
              tr.sequence(
                tr.map(row)(f)
              )
          }
          .sequence
          .map {
            make[B, fa.Row](_)(tr)
          }
      }

      override def foldLeft[A, B](fa: Vectors[A], b: B)(f: (B, A) ⇒ B): B = {
        implicit val tr = fa.traverseRow
        fa
          .rows
          .foldLeft(b) {
            (b, row) ⇒
              tr.foldLeft(row, b)(f)
          }
      }

      override def foldRight[A, B](fa: Vectors[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = {
        implicit val tr = fa.traverseRow
        fa
          .rows
          .foldRight(lb) {
            (row, lb) ⇒
              tr.foldRight(row, lb)(f)
          }
      }
    }

  type Nested[A, Row[_]] = Vector[Row[A]]

  def makeVectorTraverse[Row[U]](implicit tr: Traverse[Row]): Traverse[Nested[?, Row]] = {
    type V[A] = Nested[A, Row]
    new Traverse[V] {
      def traverse[G[_], A, B](fa: V[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[V[B]] =
        fa
          .map {
            row ⇒
              tr.traverse(row)(f)
          }
          .sequence
      def foldLeft[A, B](fa: V[A], b: B)(f: (B, A) ⇒ B): B = ???
      def foldRight[A, B](fa: V[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
    }
  }

  /**
   * Non-implicit version of [[traverse]] that makes [[Row]]-type explicit; necessary in some cases (in this file) to
   * make the compiler happy
   */
  def makeTraverse[
    Row[U]
  ]:
    Traverse[
      Aux[?, Row]
    ] =
    new Traverse[Aux[?, Row]] {
      type C[T] = Aux[T, Row]
      override def traverse[G[_], A, B](fa: C[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[C[B]] = {
        implicit val tr = fa.traverseRow
        fa
          .rows
          .map {
            row ⇒
              tr.sequence(
                tr.map(row)(f)
              )
          }
          .sequence
          .map { make[B, fa.Row](_)(tr) }
      }

      override def foldLeft[A, B](fa: C[A], b: B)(f: (B, A) ⇒ B): B = {
        implicit val tr = fa.traverseRow
        fa
          .rows
          .foldLeft(b) {
            (b, row) ⇒
              tr.foldLeft(row, b)(f)
          }
      }

      override def foldRight[A, B](fa: C[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = {
        implicit val tr = fa.traverseRow
        fa
          .rows
          .foldRight(lb) {
            (row, lb) ⇒
              tr.foldRight(row, lb)(f)
          }
      }
    }

  implicit val vector1Traverse: Traverse[Vector] =
    new Traverse[Vector] {
      def traverse[G[_], A, B](fa: Vector[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[Vector[B]] = {
        var builder = ev.pure { Vector.newBuilder[B] }
        var idx = 0
        while (idx < fa.length) {
          ev.map2(builder, f(fa(idx))) { _ += _ }
          idx += 1
        }
        ev.map(builder) { _.result() }
      }
      @inline def foldLeft [A, B](fa: Vector[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  = fa.foldLeft ( b)(f)
      @inline def foldRight[A, B](fa: Vector[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.foldRight(lb)(f)
    }

  // TODO: manually unroll 6 dimesions' worth for now; couldn't get implicit derivations to work.
  // HOPE: reversing the order of the type-params in [[Aux]] will allow partial-unification to actually work and make
  // many of these cases smoother, allow removing the Vectors.traverseRow member, etc.

  type Vector1[T] = Vector[T]
  type Vector2[T] = Vectors.Aux[T, Vector1]
  type Vector3[T] = Vectors.Aux[T, Vector2]
  type Vector4[T] = Vectors.Aux[T, Vector3]
  type Vector5[T] = Vectors.Aux[T, Vector4]
  type Vector6[T] = Vectors.Aux[T, Vector5]

  implicit val traverseV2: Traverse[Vector2] = makeTraverse[Vector1]
  implicit val traverseV3: Traverse[Vector3] = makeTraverse[Vector2]
  implicit val traverseV4: Traverse[Vector4] = makeTraverse[Vector3]
  implicit val traverseV5: Traverse[Vector5] = makeTraverse[Vector4]
  implicit val traverseV6: Traverse[Vector6] = makeTraverse[Vector5]

  /**
   * Convenience-constructor in terms of the [[Arg]] DSL / magnet-pattern
   *
   * Any sequence of arguments whose least upper-bound [[In]] has an [[Arg]] instance can be made into a [[Vectors]]
   */
  def apply[In](args: In*)(implicit arg: Arg[In]): Aux[arg.Elem, Aux[?, arg.Row]] =
    make[
      arg.Elem,
      Aux[?, arg.Row]
    ](
      args
        .map(arg(_))
        .toVector
    )(
      makeTraverse[arg.Row]
    )

  /**
   * (Vestigial?) DSL for converting a variety of nested-[[Seq]]-like types to [[Vectors]]
   * @tparam In an "input" type that can be massaged into a [[Vectors]]
   */
  trait Arg[In] {
    type Elem
    type Row[_]
    implicit def traverseRow: Traverse[Row]
    def apply(in: In): Aux[Elem, Row]
  }
  trait LowPriArg {

    type Aux[In, _E, _R[_]] =
      Arg[In] {
        type Elem = _E
        type Row[U] = _R[U]
      }

    def make[In, _E, _R[_]](fn: In ⇒ Vectors.Aux[_E, _R])(implicit _traverseRow: Traverse[_R]) =
      new Arg[In] {
        type Elem = _E
        type Row[U] = _R[U]
        implicit val traverseRow: Traverse[_R] = _traverseRow
        @inline def apply(in: In): Vectors.Aux[_E, _R] = fn(in)
      }

    implicit def base[T]: Aux[Vector[T], T, Id] = make[Vector[T], T, Id](Vectors.make[T, Id](_))

    implicit def range[R <: Range]: Aux[R, Int, Id] = make[R, Int, Id](r ⇒ Vectors.make[Int, Id](r.toVector))
  }

  object Arg
    extends LowPriArg {
    /**
     * If [[Prev]] is an [[Arg]], then wrapping in another level of [[Seq]]-subclass gives an [[Arg]] for a [[Vectors]]
     * that is one level further nested
     */
    implicit def cons[
      Prev,
      I[U] <: Seq[U]
    ](
      implicit
      prev: Lazy[Arg[Prev]]
    ):
      Aux[
        I[Prev],
        prev.value.Elem,
        Vectors.Aux[?, prev.value.Row]
      ] =
      make[
        I[Prev],
        prev.value.Elem,
        Vectors.Aux[?, prev.value.Row]
      ](
        rows ⇒ {
          val converted: Vector[Vectors.Aux[prev.value.Elem, prev.value.Row]] =
            rows
              .map(
                prev.value(_)
              )
              .toVector

          Vectors.make[
            prev.value.Elem,
            Vectors.Aux[?, prev.value.Row]
          ](
            converted
          )(
            makeTraverse[prev.value.Row]
          )
        }
      )(
        makeTraverse[prev.value.Row]
      )
  }
}
