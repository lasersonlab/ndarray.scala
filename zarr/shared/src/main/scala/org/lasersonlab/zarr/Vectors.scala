package org.lasersonlab.zarr

import cats.{ Applicative, Eval, Traverse }
import shapeless.Lazy
import shapeless.Id

case class Vectors[T, Row[U]](rows: Vector[Row[T]]) {
  type Rows = Vector[Row[T]]
  val size = rows.length
  def apply(idx: Int): Row[T] = rows(idx)
}
object Vectors {
//  implicit def unwrap[T, Row[U]](v: Vectors[T, Row]): v.Rows = v.rows

  def apply[In](args: In*)(implicit arg: Arg[In]): Vectors[arg.Elem, Vectors[?, arg.Row]] =
    new Vectors[
      arg.Elem,
      Vectors[?, arg.Row]
    ](
      args
        .map(arg(_))
        .toVector
    )

  import shapeless.{ Lazy, the }
  sealed trait IsSeqs[R[T]]
  object IsSeqs {
    implicit val id: IsSeqs[Id] = new IsSeqs[Id] {}
    implicit def rec[R[T]](implicit r: Lazy[IsSeqs[R]]): IsSeqs[λ[A ⇒ Seq[R[A]]]] = new IsSeqs[λ[A ⇒ Seq[R[A]]]] {}
  }
  the[IsSeqs[Id]]  // compiles
  IsSeqs.rec[Id]   // compiles
//  the[IsSeqs[Seq]]  // could not find implicit value for parameter t: IsSeqs[Seq]
//  the[IsSeqs[λ[X ⇒ Seq[Id[X]]]]]  // could not find implicit value for parameter t: IsSeqs[[A]Seq[A]]

  import cats.implicits._
  implicit def traverse[
    Row[U]
  ](
    implicit
    traverseRow: Traverse[Row]
  ):
    Traverse[
      Vectors[?, Row]
    ] =
    new Traverse[Vectors[?, Row]] {
      type C[T] = Vectors[T, Row]
      override def traverse[G[_], A, B](fa: C[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[C[B]] =
        fa
          .rows
          .map {
            _
              .map(f)
              .sequence
          }
          .sequence
          .map { Vectors(_) }

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

  trait Arg[In] {
    type Elem
    type Row[_]
    def apply(in: In): Vectors[Elem, Row]
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

    def make[In, _E, _R[_]](fn: In ⇒ Vectors[_E, _R]) =
      new Arg[In] {
        type Elem = _E
        type Row[U] = _R[U]
        @inline def apply(in: In): Vectors[_E, _R] = fn(in)
      }

    //type Id[T] = T
//    class Id[T](val t: T) extends AnyVal
    implicit def base[T]: Aux[Vector[T], T, Id] = make[Vector[T], T, Id](new Vectors[T, Id](_))
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
        Vectors[?, prev.value.Row]
      ] =
      make[
        Vector[Prev],
        Elem,
        Vectors[?, prev.value.Row]
      ] {
        rows ⇒
          val converted =
            rows
              .map(
                prev.value(_)
              )
          new Vectors[
            Elem,
            Vectors[?, prev.value.Row]
          ](
            converted
          )
      }
  }
}
