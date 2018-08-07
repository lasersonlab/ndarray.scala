package org.lasersonlab.ndarray.again

import cats.{ Applicative, Eval, Functor, Traverse }
import shapeless.Lazy

sealed trait Array[T]

case class Atom[T](value: T) extends Array[T]
object Atom {
  implicit val functor: Functor[Atom] =
    new Functor[Atom] {
      def map[A, B](fa: Atom[A])(f: A ⇒ B): Atom[B] = Atom(f(fa.value))
    }

  implicit val traverse: Traverse[Atom] =
    new Traverse[Atom] {
      def traverse[G[_], A, B](fa: Atom[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[Atom[B]] =
        ev.map(f(fa.value))(Atom(_))

      def foldLeft[A, B](fa: Atom[A], b: B)(f: (B, A) ⇒ B): B = f(b, fa.value)

      def foldRight[A, B](fa: Atom[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = f(fa.value, lb)
    }
}

abstract class Cns[T]
  extends Array[T] {
  type Row[U] <: Array[U]
  type Rows[_]
  implicit def f: Functor[Rows]
  def size: Int
  def rows: Rows[Row[T]]
  def apply(idx: Int): Row[T]
}
object Cns {
  type Aux[T, _Rows[_], _Row[_]] =
    Cns[T] {
      type Row [U] = _Row [U]
      type Rows[U] = _Rows[U]
    }
  type  Ax[T, _Row[_]] = Cns[T] { type Row[U] = _Row[U] }
}

case class Cons[T, _Row[U] <: Array[U]](rows: Vector[_Row[T]])
  extends Cns[T] {
  type Row[U] = _Row[U]
  type Rows[U] = Vector[U]
  override implicit def f: Functor[Vector] = Cons.vectorFunctor
  val size = rows.length
  def apply(idx: Int): Row[T] = rows(idx)
}
object Cons {
  implicit val vectorFunctor: Functor[Vector] =
    new Functor[Vector] {
      override def map[A, B](fa: Vector[A])(f: A ⇒ B): Vector[B] = fa.map(f)
    }

  import cats.implicits._
  implicit def traverse[
    Row[U] <: Array[U]
  ](
    implicit
    traverseRow: Traverse[Row]
  ):
    Traverse[
      Cons[?, Row]
    ] =
    new Traverse[Cons[?, Row]] {
      type C[T] = Cons[T, Row]
      override def traverse[G[_], A, B](fa: C[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[C[B]] =
        fa
          .rows
          .map {
            _
              .map(f)
              .sequence
          }
          .sequence
          .map { Cons(_) }

      override def foldLeft[A, B](fa: C[A], b: B)(f: (B, A) ⇒ B): B =
        fa
          .rows
          .foldLeft(b) {
            (b, row) ⇒
              row.foldLeft(b)(f)
          }

      override def foldRight[A, B](fa: C[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] =
        fa
          .rows
          .foldRight(lb) {
            (row, lb) ⇒
              row.foldRight(lb)(f)
          }
    }
}

trait Filled[T, A <: Array[T]]{
  def apply(value: T): A
}

case class Sparse[T, _Row[U] <: Array[U]](
  rows: Map[Int, _Row[T]],
  size: Int,
  fill: T
)(
  implicit val filled: Filled[T, _Row[T]]
)
extends Cns[T] {
  type Row[U] = _Row[U]
  type Rows[U] = Map[Int, U]

  override implicit def f: Functor[Map[Int, ?]] = Sparse.intMapFunctor

  val empty = filled(fill)
  def apply(idx: Int): Row[T] =
    rows
      .getOrElse(
        idx,
        empty
      )
}
object Sparse {
  implicit val intMapFunctor: Functor[Map[Int, ?]] =
    new Functor[Map[Int, ?]] {
      override def map[A, B](fa: Map[Int, A])(f: A ⇒ B): Map[Int, B] = fa.mapValues(f)
    }
}


object Array {
//  implicit val functor: Functor[Array] =
//    new Functor[Array] {
//      def map[A, B](fa: Array[A])(f: A ⇒ B): Array[B] =
//        fa match {
//          case Atom(value) ⇒ Atom(f(value))
//          case Cons(rows) ⇒
//        }
//    }

//  implicit def rowsFunctor[
//    T,
//    _Row <: Array[T]
//  ](
//    implicit
//    f: Functor[_Row]
//  ):
//    Functor[
//      _Row
//    ]

  trait Arg[In] {
    type T
    type A[U] <: Array[U]
    def apply(in: In): A[T]
  }
  trait LowPriArg {

    type Ax[In, _T] =
      Arg[In] {
        type T = _T
      }

    type Aux[In, _T, _A[U] <: Array[U]] =
      Arg[In] {
        type T = _T
        type A[U] = _A[U]
      }

    def make[In, _T, _A[U] <: Array[U]](fn: In ⇒ _A[_T]) =
      new Arg[In] {
        type T = _T
        type A[U] = _A[U]
        @inline def apply(in: In): A[T] = fn(in)
      }

    implicit def atom[T]: Aux[T, T, Atom] = make[T, T, Atom](Atom(_))
  }
  object Arg
    extends LowPriArg {
    implicit def cons[
      Prev,
      T
    ](
      implicit
      prev: Lazy[Ax[Prev, T]]
    ):
      Aux[
        Vector[Prev],
        T,
        Cons[?, prev.value.A]
      ] =
      make[
        Vector[Prev],
        T,
        Cons[?, prev.value.A]
      ] {
        rows ⇒
          val converted =
            rows
              .map(
                prev.value(_)
              )
          Cons[T, prev.value.A](
            converted
          )
      }
  }

  def apply[T](t: T)(implicit arg: Arg[T]): arg.A[arg.T] = arg(t)

  implicit def consFunctor[
    R[U] <: Array[U]
  ](
    implicit
    fr: Functor[R]
  ):
    Functor[
      λ[E ⇒ Cns.Ax[E, R]]
    ] =
    new Functor[
      λ[E ⇒ Cns.Ax[E, R]]
    ] {
      override def map[A, B](fa: Cns.Ax[A, R])(fn: A ⇒ B): Cns.Ax[B, R] =
        fa match {
          case Cons(rows) ⇒
            Cons[B, R](
              rows
                .map(fr.map(_)(fn))
            )
          case s @ Sparse(map, size, fill) ⇒
            Sparse[B, R](
              map.mapValues(fr.map(_)(fn)),
              size,
              fn(fill)
            )(
              new Filled[B, R[B]] {
                override def apply(value: B): R[B] = fr.map(s.empty)(fn)
              }
            )
          case _ ⇒
            new Cns[B] {
              type Row[U] = R[U]
              type Rows[U] = fa.Rows[U]

              override implicit def f: Functor[fa.Rows] = fa.f

              def size = fa.size
              def rows =
                fa.f.map(fa.rows) {
                  row ⇒
                    fr.map(row)(fn)
                }
              def apply(idx: Int): Row[B] = fr.map(fa(idx))(fn)
            }
        }
    }
}
