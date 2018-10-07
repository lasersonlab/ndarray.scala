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
  type Aux[_R[_], T] = Vectors[T] { type Row[U] = _R[U] }

  /**
   * Convenience constructor for [[Vectors]] instances
   */
  def make[
    _Row[_],
       T
  ](
    _rows: Vector[_Row[T]]
  )(
    implicit
    _traverseRow: Traverse[_Row]
  ):
    Aux[_Row, T] =
    new Vectors[T] {
      type Row[U] = _Row[U]
      val rows = _rows
      override val size = rows.size
      val traverseRow = _traverseRow
    }

  implicit val traverse: Traverse[Vectors] =
    new Traverse[Vectors] {
      override def traverse[
        G[_]: Applicative,
        A,
        B
      ](
        fa: Vectors[A]
      )(
        f: A ⇒ G[B]
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
            make[fa.Row, B](_)(tr)
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

  /**
   * Non-implicit version of [[traverse]] that makes [[Row]]-type explicit; necessary in some cases (in this file) to
   * make the compiler happy
   */
  def makeTraverse[
    Row[_]
  ]:
    Traverse[
      Aux[Row, ?]
    ] =
    new Traverse[Aux[Row, ?]] {
      type F[T] = Aux[Row, T]
      override def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A ⇒ G[B]): G[F[B]] = {
        implicit val tr: Traverse[Row] = fa.traverseRow
        fa
          .rows
          .map {
            _
              .map(f)
              .sequence
          }
          .sequence
          .map { make[Row, B] }
      }

      override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B = {
        implicit val tr: Traverse[Row] = fa.traverseRow
        fa
          .rows
          .foldLeft(b) {
            (b, row) ⇒
              row.foldLeft(b)(f)
          }
      }

      override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = {
        implicit val tr: Traverse[Row] = fa.traverseRow
        fa
          .rows
          .foldRight(lb) {
              _.foldRight(_)(f)
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

  // HACK: manually unroll 6 dimesions' worth for now; couldn't get implicit derivations to work, perhaps due to
  // https://github.com/scala/bug/issues/11169

  type Vector1[T] = Vector[T]
  type Vector2[T] = Aux[Vector1, T]
  type Vector3[T] = Aux[Vector2, T]
  type Vector4[T] = Aux[Vector3, T]
  type Vector5[T] = Aux[Vector4, T]
  type Vector6[T] = Aux[Vector5, T]

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
  def apply[In](args: In*)(implicit arg: Arg[In]): Aux[Aux[arg.Row, ?], arg.Elem] =
    make[
      Aux[arg.Row, ?],
      arg.Elem
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
    def apply(in: In): Aux[Row, Elem]
  }
  trait LowPriArg {

    type Aux[In, _E, _R[_]] =
      Arg[In] {
        type Elem = _E
        type Row[U] = _R[U]
      }

    def make[In, _E, _R[_]](fn: In ⇒ Vectors.Aux[_R, _E])(implicit _traverseRow: Traverse[_R]) =
      new Arg[In] {
        type Elem = _E
        type Row[U] = _R[U]
        implicit val traverseRow: Traverse[_R] = _traverseRow
        @inline def apply(in: In): Vectors.Aux[_R, _E] = fn(in)
      }

    implicit def base[T]: Aux[Vector[T], T, Id] = make[Vector[T], T, Id](Vectors.make[Id, T])

    implicit def range[R <: Range]: Aux[R, Int, Id] = make[R, Int, Id](r ⇒ Vectors.make[Id, Int](r.toVector))
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
        Vectors.Aux[prev.value.Row, ?]
      ] =
      make[
        I[Prev],
        prev.value.Elem,
        Vectors.Aux[prev.value.Row, ?]
      ](
        rows ⇒ {
          val converted: Vector[Vectors.Aux[prev.value.Row, prev.value.Elem]] =
            rows
              .map(
                prev.value(_)
              )
              .toVector

          Vectors.make[
            Vectors.Aux[prev.value.Row, ?],
            prev.value.Elem
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
