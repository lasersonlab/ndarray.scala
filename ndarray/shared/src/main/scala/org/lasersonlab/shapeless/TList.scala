package org.lasersonlab.shapeless

import cats.{ Applicative, Eval, Reducible, Semigroupal, Traverse }
import org.lasersonlab.ndarray.Scannable

trait TList[T] {
  def head: T
  type Tail
  def tail: Tail
  def ::[_Tail >: this.type <: TList[T]](h: T): TList.::[T, _Tail] = {
    val self: _Tail = this
    new TList[T] {
      type Tail = _Tail
      val head = h
      val tail = self
    }
  }
}
object TList {

  type Aux[_Tail, T] = TList[T] { type Tail = _Tail }
  type  ::[T, _Tail] = TList[T] { type Tail = _Tail }

  object :: {
    def apply[T, _Tail](h: T, t: _Tail): T :: _Tail =
      new TList[T] {
        val head: T = h
        type Tail = _Tail
        val tail: Tail = t
      }
    def unapply[T](l: TList[T]): Option[(T, l.Tail)] =
      Some(
        (
          l.head,
          l.tail
        )
      )
  }

  case object `0` {
    def ::[T](h: T): `1`[T] = TList.::(h, `0`)
  }
  val ⊥ = `0`
  type ⊥ = `0`.type
  type `0`[T] = `0`.type

  type `1`[T] = T :: `0`[T]
  type `2`[T] = T :: `1`[T]
  type `3`[T] = T :: `2`[T]
  type `4`[T] = T :: `3`[T]
  type `5`[T] = T :: `4`[T]
  type `6`[T] = T :: `5`[T]

  trait Base[F[_]]
    extends    Traverse[F]
       with Semigroupal[F]
       with   Scannable[F]

  trait Utils[F[_]]
    extends      Base[F]
       with Reducible[F]

  trait instances {
    import cats.implicits._

    private def cons[Tail[_]](implicit tail: Base[Tail]): Utils[λ[T ⇒ Aux[Tail[T], T]]] = {
      type F[T] = Aux[Tail[T], T]
      val tl = tail
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


        override def reduceLeftTo [A, B](fa: F[A])(f: A ⇒ B)(g: (B, A) ⇒ B): B =
          fa
            .tail
            .foldLeft(
              f(
                fa.head
              )
            )(
              g
            )

        import Eval.always
        override def reduceRightTo[A, B](fa: F[A])(f: A ⇒ B)(g: (A, Eval[B]) ⇒ Eval[B]): Eval[B] =
          fa
            .tail
            .foldRight(
              Eval.always[Option[B]](None)
            ) {
              (a, lb) ⇒
                lb
                  .flatMap {
                    case None    ⇒ always(Some(f(a)))
                    case Some(b) ⇒ g(a, always(b)).map(Some(_))
                  }
            }
            .flatMap {
              case None ⇒ always(f(fa.head))
              case Some(b) ⇒ g(fa.head, always(b))
            }

        override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B =
          fa
            .tail
            .foldLeft(
              f(
                b,
                fa.head
              )
            )(
              f
            )

        override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] =
          f(
            fa.head,
            fa
              .tail
              .foldRight(lb)(f)
          )

        override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
          (fa, fb) match {
            case (
              ha :: ta,
              hb :: tb
            ) ⇒
              ::(
                (ha, hb),
                ta.product(tb)
              )
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

    implicit val utils0 =
      new Base[`0`] {
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
}

//trait Next[TL[_]] {
//  type Next[_]
//}
//object Next {
//  type Aux[TL[_], N[_]] = Next[TL] { type Next[U] = N[U] }
//  implicit val `0`: Aux[TList.`0`, TList.`1`] = ???
//}
