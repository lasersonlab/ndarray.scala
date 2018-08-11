package org.lasersonlab.zarr

import cats.{ Applicative, Eval, Traverse }
import cats.implicits._
import shapeless.{ Id, Lazy }

trait Vctrs[T] {
  type Row[U]
  def rows: Vector[Row[T]]
  def size = rows.length
  implicit def traverseRow: Traverse[Row]
  def apply(idx: Int): Row[T] = rows(idx)
}

object Vctrs {
  type Aux[T, _R[_]] = Vctrs[T] { type Row[U] = _R[U] }

  def make[T, _Row[U]](_rows: Vector[_Row[T]])(implicit _traverseRow: Traverse[_Row]): Aux[T, _Row] =
    new Vctrs[T] {
      type Row[U] = _Row[U]
      val rows = _rows
      val traverseRow = _traverseRow
    }

  implicit val traverse: Traverse[Vctrs] =
    new Traverse[Vctrs] {
      override def traverse[G[_], A, B](fa: Vctrs[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[Vctrs[B]] = {
        implicit val traverseRow = fa.traverseRow
        fa
          .rows
          .map {
            row ⇒
              traverseRow.sequence(
                traverseRow.map(row)(f)
              )
          }
          .sequence
          .map {
            make[B, fa.Row](_)(traverseRow)
          }
      }

      override def foldLeft[A, B](fa: Vctrs[A], b: B)(f: (B, A) ⇒ B): B = {
        implicit val traverseRow = fa.traverseRow
        fa
          .rows
          .foldLeft(b) {
            (b, row) ⇒
              traverseRow.foldLeft(row, b)(f)
          }
      }

      override def foldRight[A, B](fa: Vctrs[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = {
        implicit val traverseRow = fa.traverseRow
        fa
          .rows
          .foldRight(lb) {
            (row, lb) ⇒
              traverseRow.foldRight(row, lb)(f)
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

    type Ax[In, _E] =
      Arg[In] {
        type Elem = _E
      }

    def make[In, _E, _R[_]](fn: In ⇒ Vctrs.Aux[_E, _R])(implicit _traverseRow: Traverse[_R]) =
      new Arg[In] {
        type Elem = _E
        type Row[U] = _R[U]
        implicit val traverseRow: Traverse[_R] = _traverseRow
        @inline def apply(in: In): Vctrs.Aux[_E, _R] = fn(in)
      }

    implicit def base[T]: Aux[Vector[T], T, Id] = make[Vector[T], T, Id](Vctrs.make[T, Id](_))
  }

  object Arg
    extends LowPriArg {
    implicit def cons[
      Prev,
      Elem
    ](
      implicit
      prev: Lazy[Ax[Prev, Elem]]
    ):
      Aux[
        Vector[Prev],
        Elem,
        Vctrs.Aux[?, prev.value.Row]
      ] =
      make[
        Vector[Prev],
        Elem,
        Vctrs.Aux[?, prev.value.Row]
      ](
        rows ⇒ {
          val converted: Vector[Vctrs.Aux[Elem, prev.value.Row]] =
            rows
              .map(
                prev.value(_)
              )

          Vctrs.make[
            Elem,
            Vctrs.Aux[?, prev.value.Row]
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
