package org.lasersonlab.ndarray

import cats.{ Applicative, Eval, Traverse }
import cats.implicits._
import shapeless.{ Id, Lazy }

trait Vectors[T] {
  type Row[U]
  def rows: Vector[Row[T]]
  def size = rows.length
  implicit def traverseRow: Traverse[Row]
  def apply(idx: Int): Row[T] = rows(idx)
}

object Vectors {
  type Aux[T, _R[_]] = Vectors[T] { type Row[U] = _R[U] }

  def make[T, _Row[U]](_rows: Vector[_Row[T]])(implicit _traverseRow: Traverse[_Row]): Aux[T, _Row] =
    new Vectors[T] {
      type Row[U] = _Row[U]
      val rows = _rows
      val traverseRow = _traverseRow
    }

  implicit val traverse: Traverse[Vectors] =
    new Traverse[Vectors] {
      override def traverse[G[_], A, B](fa: Vectors[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[Vectors[B]] = {
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

  type Vector0[T] = Id[T]
  type Vector1[T] = Vectors.Aux[T, Id]
  type Vector2[T] = Vectors.Aux[T, Vector1]
  type Vector3[T] = Vectors.Aux[T, Vector2]

  implicit val traverseV1: Traverse[Vector1] = makeTraverse[Id]
  implicit val traverseV2: Traverse[Vector2] = makeTraverse[Vector1]
  implicit val traverseV3: Traverse[Vector3] = makeTraverse[Vector2]

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
