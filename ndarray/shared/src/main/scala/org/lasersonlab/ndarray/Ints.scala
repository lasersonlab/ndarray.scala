package org.lasersonlab.ndarray

import cats.{ Applicative, Eval, Semigroupal, Traverse }
import cats.implicits._
import shapeless.{ Nat, Succ }
import org.lasersonlab.ndarray.TList._

trait Shape[N <: Nat] {
  type F[T] <: hammerlab.shapeless.tlist.TList
  implicit val utils: Utils[F]
}
object Shape {
  import hammerlab.shapeless.tlist._
  implicit val tnil: Aux[shapeless._0, _0] =
    new Shape[shapeless._0] {
      type F[T] = _0[T]
      override implicit val utils: Utils[F] =
        new Utils[F] {
          def traverse[G[_], A, B](fa: F[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[F[B]] = ev.pure(TNil)
          def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B = b
          def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = lb
          def scanLeft[A, B](fa: F[A], b: B, f: (B, A) ⇒ B): (F[B], B) = (TNil, b)
          def scanRight[A, B](fa: F[A], b: B, f: (A, B) ⇒ B): (B, F[B]) = (b, TNil)
          def product[A, B](fa: _0[A], fb: _0[B]): _0[(A, B)] = TNil
        }
    }

  type Aux[N <: Nat, _F[_] <: TList] = Shape[N] { type F[T] = _F[T] }

  implicit def cons[
    Prev <: Nat
  ](
    implicit
    tail: Shape[Prev]
  ):
    Aux[
      Succ[Prev],
      λ[
        T ⇒
        T :: tail.F[T]
      ]
    ] =
    new Shape[Succ[Prev]] {
      type F[T] = T :: tail.F[T]

      val tl = tail.utils

      implicit val utils: Utils[F] =
        new Utils[F] {
          override def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A ⇒ G[B]): G[F[B]] =
            fa match {
              case h :: t ⇒
                f(h)
                  .map2(
                    tl.traverse(t)(f)
                  ) {
                    ::(_, _)
                  }
            }

          override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B =
            tl
              .foldLeft(
                fa.tail,
                f(b, fa.head)
              )(
                f
              )

          override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???

          override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
            (fa, fb) match {
              case (ha :: ta, hb :: tb) ⇒
                ::((ha, hb), tl.product(ta, tb))
            }

          override def scanLeft[A, B](fa: F[A], b: B, f: (B, A) ⇒ B): (F[B], B) = {
            val next = f(b, fa.head)
            val (scanned, total) = tl.scanLeft(fa.tail, next, f)
            (
              ::(b, scanned),
              total
            )
          }

          override def scanRight[A, B](fa: F[A], b: B, f: (A, B) ⇒ B): (B, F[B]) = {
            val (subtotal, tail) = tl.scanRight(fa.tail, b, f)
            (
              f(
                fa.head,
                subtotal
              ),
              ::(subtotal, tail)
            )
          }
        }
    }
}

object TList {
  import hammerlab.shapeless.tlist._
  type `0`[T] = TNil
  type `1`[T] = T :: `0`[T]
  type `2`[T] = T :: `1`[T]
  type `3`[T] = T :: `2`[T]
  type `4`[T] = T :: `3`[T]
  type `5`[T] = T :: `4`[T]
  type `6`[T] = T :: `5`[T]

  type _0[T] = TNil
  type _1[T] = T :: _0[T]
  type _2[T] = T :: _1[T]
  type _3[T] = T :: _2[T]
  type _4[T] = T :: _3[T]
  type _5[T] = T :: _4[T]
  type _6[T] = T :: _5[T]

  import Shape._

  trait Utils[F[_]]
    extends    Traverse[F]
       with Semigroupal[F]
       with   Scannable[F]

  trait traverses {
    import shapeless.the
    implicit val traverse_0: Utils[`0`] = the[Shape.Aux[shapeless.nat._0, _0]].utils
    implicit val traverse_1: Utils[`1`] = the[Shape.Aux[shapeless.nat._1, _1]].utils
    implicit val traverse_2: Utils[`2`] = the[Shape.Aux[shapeless.nat._2, _2]].utils
    implicit val traverse_3: Utils[`3`] = the[Shape.Aux[shapeless.nat._3, _3]].utils
    implicit val traverse_4: Utils[`4`] = the[Shape.Aux[shapeless.nat._4, _4]].utils
    implicit val traverse_5: Utils[`5`] = the[Shape.Aux[shapeless.nat._5, _5]].utils
    implicit val traverse_6: Utils[`6`] = the[Shape.Aux[shapeless.nat._6, _6]].utils
  }
  object traverses extends traverses
}

object Ints {
  import hammerlab.shapeless.tlist._
  type Ints1 = Int ::  TNil
  type Ints2 = Int :: Ints1
  type Ints3 = Int :: Ints2
  type Ints4 = Int :: Ints3
  type Ints5 = Int :: Ints4
  type Ints6 = Int :: Ints5
}
