package org.lasersonlab.slist

import cats.implicits._
import cats.{ Applicative, Eval, Reducible, Traverse }
import hammerlab.either._
import org.lasersonlab.slist.SList.FromList.{ Err, TooFew, TooMany }
import org.lasersonlab.slist.Scannable.syntax._
import org.lasersonlab.slist.Zip.syntax._

trait SList {
  type Head
  def head: Head
  type Tail[_]
  def tail: Tail[Head]
  def size: Int
}

object SList {

  type Aux[T, _Tail[_]] = SList { type Head = T; type Tail[U] = _Tail[U] }

  object :: {
    def unapply[
      L <: SList
    ](
      l: L
    ):
      Option[
        (
          l.Head,
          l.Tail[
            l.Head
          ]
        )
      ] =
      Some(
        (
          l.head,
          l.tail
        )
      )
  }

  case object `0` {
    def ::[T](h: T) = `1`(h)
    def size = 0
  }
  val   ⊥     = `0`
  type  ⊥     = `0`.type
  type `0`[T] = `0`.type

  case class `1`[T](head: T              ) extends SList { type Head = T; type Tail[U] = `0`[U]; def size = 1 ; def tail: `0`[T] = `0` }
  case class `2`[T](head: T, tail: `1`[T]) extends SList { type Head = T; type Tail[U] = `1`[U]; def size = 2 }
  case class `3`[T](head: T, tail: `2`[T]) extends SList { type Head = T; type Tail[U] = `2`[U]; def size = 3 }
  case class `4`[T](head: T, tail: `3`[T]) extends SList { type Head = T; type Tail[U] = `3`[U]; def size = 4 }
  case class `5`[T](head: T, tail: `4`[T]) extends SList { type Head = T; type Tail[U] = `4`[U]; def size = 5 }
  case class `6`[T](head: T, tail: `5`[T]) extends SList { type Head = T; type Tail[U] = `5`[U]; def size = 6 }
  case class `7`[T](head: T, tail: `6`[T]) extends SList { type Head = T; type Tail[U] = `6`[U]; def size = 7 }
  case class `8`[T](head: T, tail: `7`[T]) extends SList { type Head = T; type Tail[U] = `7`[U]; def size = 8 }
  case class `9`[T](head: T, tail: `8`[T]) extends SList { type Head = T; type Tail[U] = `8`[U]; def size = 9 }

  /**
   * Type-class for prepending an element to an [[SList]]
   */
  trait Cons[In[_]] {
    type Out[T] <: Aux[T, In]
    def apply[T](h: T, t: In[T]): Out[T]
  }
  object Cons {
    abstract class Aux[In[_], O[U] <: SList.Aux[U, In]]
      extends Cons[In] {
      type Out[U] = O[U]
    }
    implicit val cons_0 = new Aux[`0`, `1`] { def apply[T](h: T, t: `0`[T]) = `1`(h   ) }
    implicit val cons_1 = new Aux[`1`, `2`] { def apply[T](h: T, t: `1`[T]) = `2`(h, t) }
    implicit val cons_2 = new Aux[`2`, `3`] { def apply[T](h: T, t: `2`[T]) = `3`(h, t) }
    implicit val cons_3 = new Aux[`3`, `4`] { def apply[T](h: T, t: `3`[T]) = `4`(h, t) }
    implicit val cons_4 = new Aux[`4`, `5`] { def apply[T](h: T, t: `4`[T]) = `5`(h, t) }
    implicit val cons_5 = new Aux[`5`, `6`] { def apply[T](h: T, t: `5`[T]) = `6`(h, t) }
    implicit val cons_6 = new Aux[`6`, `7`] { def apply[T](h: T, t: `6`[T]) = `7`(h, t) }
    implicit val cons_7 = new Aux[`7`, `8`] { def apply[T](h: T, t: `7`[T]) = `8`(h, t) }
    implicit val cons_8 = new Aux[`8`, `9`] { def apply[T](h: T, t: `8`[T]) = `9`(h, t) }
  }

  implicit class Ops[T, Tail[_]](val tail: Tail[T]) extends AnyVal {
    def ::(head: T)(implicit cons: Cons[Tail]): cons.Out[T] = cons(head, tail)
    def size(implicit base: Base[Tail]): Int = base.size
  }

  /**
   * Attempt to convert a [[List]] to an [[F]], raising an [[FromList.Err error]] is there are too few or too many
   * elements
   */
  trait FromList[F[_]] {
    val size: Int
    def apply[T](l: List[T]): Err | F[T]
  }
  object FromList {
    sealed trait Err
    case class TooFew [T](  gap:  Int   ) extends Err
    case class TooMany[T](extra: List[T]) extends Err
  }

  /**
   * Bundle of useful implementations for all [[SList]] types, including [[`0`]]
   */
  trait Base[F[_]]
    extends    Traverse[F]
       with         Zip[F]
       with   Scannable[F]
       with    FromList[F]
       with Applicative[F]

  /**
   * Extend [[Base]] implementation bundle for non-empty [[SList]]s (from [[`1`]] up)
   */
  trait Utils[F[_]]
    extends      Base[F]
       with Reducible[F]

  /**
   * Default [[Base]] or [[Utils]] implementations for all [[SList]]s
   */
  trait instances {
    implicit val base_0 =
      new Base[`0`] {
        type F[T] = `0`[T]
        val size = 0

        def traverse[G[_], A, B](fa: F[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[F[B]] = ev.pure(⊥)

        def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  =  b
        def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = lb

        def scanLeft   [A, B](fa: F[A], b: B, f: (B, A) ⇒ B): (F[B],  B ) = (⊥, b)
        def scanLeft_→ [A, B](fa: F[A], b: B, f: (B, A) ⇒ B):  F[B]       =  ⊥
        def scanRight  [A, B](fa: F[A], b: B, f: (A, B) ⇒ B): (  B, F[B]) = (b, ⊥)
        def scanRight_←[A, B](fa: F[A], b: B, f: (A, B) ⇒ B):       F[B]  =     ⊥

        def apply[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ⊥

        def apply[T](f: `0`[T]): List[T] = Nil
        def apply[T](l: List[T]): Err | F[T] =
          l match {
            case   Nil ⇒ R(⊥)
            case extra ⇒ L(TooMany(extra))
          }

        override def pure[A](x: A): `0`[A] = ⊥

        override def ap[A, B](ff: `0`[A ⇒ B])(fa: `0`[A]): `0`[B] = ⊥
      }

    implicit val utils_1 = cons[`0`]
    implicit val utils_2 = cons[`1`]
    implicit val utils_3 = cons[`2`]
    implicit val utils_4 = cons[`3`]
    implicit val utils_5 = cons[`4`]
    implicit val utils_6 = cons[`5`]
    implicit val utils_7 = cons[`6`]
    implicit val utils_8 = cons[`7`]
    implicit val utils_9 = cons[`8`]

    def cons[Tail[_]](
      implicit
      tail: Base[Tail],
        ev: Cons[Tail]
    ):
      Utils[ev.Out]
    = {
      type F[T] = ev.Out[T]
      new Utils[F] {
        override def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A ⇒ G[B]): G[F[B]] =
          fa match {
            case h :: t ⇒
              f(h)
                .map2(
                  t.traverse(f)
                ) {
                  _ :: _
                }
          }

        override def reduceLeftTo [A, B](fa: F[A])(f: A ⇒ B)(g: (B, A) ⇒ B): B =
          fa
            .tail
            .foldLeft(
              f(
                fa.head
              )
            ) {
              g
            }

        import Eval.always
        override def reduceRightTo[A, B](fa: F[A])(f: A ⇒ B)(g: (A, Eval[B]) ⇒ Eval[B]): Eval[B] =
          fa
            .tail
            .foldRight(
              always[Option[B]](None)
            ) {
              (a, lb) ⇒
                lb
                  .flatMap {
                    case None    ⇒ always(Some(f(a)))
                    case Some(b) ⇒ g(a, always(b)).map(Some(_))
                  }
            }
            .flatMap {
              case None    ⇒ always(f(fa.head))
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
            ) {
              f
            }

        override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] =
          f(
            fa.head,
            fa
              .tail
              .foldRight(lb)(f)
          )

        override def apply[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
          (fa, fb) match {
            case (
              ha :: ta,
              hb :: tb
            ) ⇒
              (ha, hb) ::
              ta.zip(tb)
          }

        override def scanLeft[A, B](fa: F[A], b: B, f: (B, A) ⇒ B): (F[B], B) = {
          val next = f(b, fa.head)
          val (scanned, total) = fa.tail.scanLeft(next)(f)
          (
            b :: scanned,
            total
          )
        }

        override def scanRight[A, B](fa: F[A], b: B, f: (A, B) ⇒ B): (B, F[B]) = {
          val (subtotal, _tail) = fa.tail.scanRight[B](b)(f)
          (
            f(
              fa.head,
              subtotal
            ),
            subtotal :: _tail
          )
        }

        override def scanLeft_→[A, B](fa: F[A], b: B, f: (B, A) ⇒ B): F[B] = {
          val next = f(b, fa.head)
          next :: fa.tail.scanLeft_→(next)(f)
        }

        override def scanRight_←[A, B](fa: F[A], b: B, f: (A, B) ⇒ B): F[B] =
          f(fa.head, b) :: fa.tail.scanRight_←(b)(f)

        val size = tail.size + 1

        override def apply[T](l: List[T]): Err | F[T] =
          l match {
            case Nil ⇒ L(TooFew(size))
            case scala.::(h, t) ⇒
              tail(t).map { h :: _ }
          }

        override def pure[A](x: A): F[A] = x :: x.pure[Tail]

        override def ap[A, B](ff: F[A ⇒ B])(fa: F[A]): F[B] =
          (ff, fa) match {
            case (
              hf :: tf,
              ha :: ta
            ) ⇒
              hf(ha) :: tf.ap(ta)
          }
      }
    }
  }
  object instances extends instances
}
