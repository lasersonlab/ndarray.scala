package org.lasersonlab.zarr

import cats.{ Applicative, Eval, Traverse }
import cats.implicits._
import org.lasersonlab.zarr.Vectors.Arg
import shapeless.{ Id, Lazy }

trait Vctrs[T] {
  type Row[U]
  def rows: Vector[Row[T]]
  implicit def traverseRow: Traverse[Row]
}

object Vctrs {
  type Aux[T, _R[_]] = Vctrs[T] { type Row[U] = _R[U] }

  def make[T, _Row[U]](_rows: Vector[_Row[T]])(implicit _traverseRow: Traverse[_Row]): Aux[T, _Row] =
    new Vctrs[T] {
      type Row[U] = _Row[U]
      val rows = _rows
      val traverseRow = _traverseRow
    }

  def apply[In](args: In*)(implicit arg: Arg[In]): Vctrs[arg.Elem] =
    new Vctrs[arg.Elem] {
      type Row[U] = Aux[U, arg.Row]
      implicit val traverseRow: Traverse[Row] = Vectors.makeTraverse[arg.Row]
      val rows: Vector[Row[arg.Elem]] =
        args
          .map(arg(_))
          .toVector
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
//            v ⇒
            make[B, fa.Row](_)(traverseRow)
//              new Vctrs[B] {
//                override type Row[U] = fa.Row[U]
//                override implicit val traverseRow: Traverse[Row] = fa.traverseRow
//                override def rows: Vector[Row[B]] = v
//              }
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
}

//case class Vectors[T, _Row[U]](rows: Vector[_Row[T]])(implicit val traverseRow: Traverse[_Row])
//  extends Vctrs[T] {
//  type Row[U] = _Row[U]
//  type Rows = Vector[_Row[T]]
//  val size = rows.length
//  def apply(idx: Int): _Row[T] = rows(idx)
//}

object Vectors {
//  implicit def unwrap[T, Row[U]](v: Vectors[T, Row]): v.Rows = v.rows

  def apply[In](args: In*)(implicit arg: Arg[In]): Vctrs.Aux[arg.Elem, Vctrs.Aux[?, arg.Row]] =
    Vctrs.make[
      arg.Elem,
      Vctrs.Aux[?, arg.Row]
    ](
      args
        .map(arg(_))
        .toVector
    )(
      makeTraverse[arg.Row]
      //arg.traverseRow
    )

  def makeTraverse[
    Row[U]
  ]:
    Traverse[
      Vctrs.Aux[?, Row]
    ] =
    new Traverse[Vctrs.Aux[?, Row]] {
      type C[T] = Vctrs.Aux[T, Row]
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
          .map { Vctrs.make[B, fa.Row](_)(tr) }
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
    def apply(in: In): Vctrs.Aux[Elem, Row]
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
