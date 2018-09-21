package org.lasersonlab.ndarray

import cats.{ Applicative, Eval, Traverse }
import cats.implicits._
import hammerlab.shapeless.tlist._
import org.hammerlab.shapeless.tlist.TList.Aux

object TList {
  type `1`[T] = T :: TNil
  type `2`[T] = T :: `1`[T]
  type `3`[T] = T :: `2`[T]
  type `4`[T] = T :: `3`[T]
  type `5`[T] = T :: `4`[T]
  type `6`[T] = T :: `5`[T]

  type _1[T] = T :: TNil
  type _2[T] = T :: _1[T]
  type _3[T] = T :: _2[T]
  type _4[T] = T :: _3[T]
  type _5[T] = T :: _4[T]
  type _6[T] = T :: _5[T]

  implicit def traverse[T <: TList: Traverse[Aux] =
    new Traverse[Aux] {
      type F[A] = Aux[A]
      def traverse[G[_], A, B](fa: F[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[F[B]] =
        fa match {
          case TNil ⇒ TNil.pure[G]
          case h :: t ⇒
            f(h)
              .product(
                traverse(t)(f)
              )
              .map {
                case (h, t) ⇒ h :: t
              }
        }

      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B =
        fa match {
          case TNil ⇒ b
          case h :: t ⇒
            foldLeft(
              t,
              f(b, h)
            )(
              f
            )
        }

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
    }
}

object Ints {
  type Ints1 = Int ::  TNil
  type Ints2 = Int :: Ints1
  type Ints3 = Int :: Ints2
  type Ints4 = Int :: Ints3
  type Ints5 = Int :: Ints4
  type Ints6 = Int :: Ints5
}
