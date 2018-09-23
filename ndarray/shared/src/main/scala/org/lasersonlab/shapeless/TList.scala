package org.lasersonlab.shapeless

import cats.{ Applicative, Eval, Foldable }
import org.lasersonlab.shapeless.Shape.Utils

trait TList[T] {
  def head: T
  type Tail[_]
  def tail: Tail[T]
}
object TList {

  type Aux[_Tail[_], T] = TList[T] { type Tail[U] = _Tail[U] }
  type  ::[T, _Tail[_]] = TList[T] { type Tail[U] = _Tail[U] }

  object :: {
    def apply[T, _Tail[_]](h: T, t: _Tail[T]): T :: _Tail =
      new TList[T] {
        val head: T = h
        type Tail[U] = _Tail[U]
        val tail: Tail[T] = t
      }
    def unapply[T](l: TList[T]): Option[(T, l.Tail[T])] =
      Some(
        (
          l.head,
          l.tail
        )
      )
  }

  case object `0`
  val ⊥ = `0`
  type `0`[T] = `0`.type

  type `1`[T] = T :: `0`
  type `2`[T] = T :: `1`
  type `3`[T] = T :: `2`
  type `4`[T] = T :: `3`
  type `5`[T] = T :: `4`
  type `6`[T] = T :: `5`

  trait instances {
    private def cons[Tail[_]](implicit tail: Utils[Tail]): Utils[Aux[Tail, ?]] = ???

    implicit val utils0 =
      new Utils[`0`] {
        type F[T] = `0`[T]
        def traverse[G[_], A, B](fa: F[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[F[B]] = ev.pure(`0`)

        def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  =  b
        def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = lb

        def scanLeft [A, B](fa: F[A], b: B, f: (B, A) ⇒ B): (F[B],  B ) = (`0`, b)
        def scanRight[A, B](fa: F[A], b: B, f: (A, B) ⇒ B): (  B, F[B]) = (b, `0`)

        def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = `0`
      }
    implicit val utils1 = cons[`0`]
    implicit val utils2 = cons[`1`]
    implicit val utils3 = cons[`2`]
    implicit val utils4 = cons[`3`]
    implicit val utils5 = cons[`4`]
    implicit val utils6 = cons[`5`]
  }
  object instances extends instances

  import instances._
  implicitly[Foldable[`1`]]
  implicitly[Foldable[`2`]]
  implicitly[Foldable[`3`]]
  implicitly[Foldable[`4`]]
  implicitly[Foldable[`5`]]
  implicitly[Foldable[`6`]]

  // 123 :: `0`  // `1`[Int]
  // ::(123, `0`)
}

trait Next[TL[_]] {
  type Next[_]
}
object Next {
  type Aux[TL[_], N[_]] = Next[TL] { type Next[U] = N[U] }
  implicit val `0`: Aux[TList.`0`, TList.`1`] = ???
}
