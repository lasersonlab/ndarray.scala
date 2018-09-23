package org.lasersonlab.shapeless

import cats.{ Applicative, Eval, Reducible, Semigroupal, Traverse }
import cats.implicits._
import hammerlab.either._
import org.lasersonlab.ndarray.Scannable
import org.lasersonlab.shapeless.SList.FromList.{ Err, TooFew, TooMany }

trait SList {
  type Head
  def head: Head
  type Tail[_]
  def tail: Tail[Head]
}

object SList {

  type Aux[T, _Tail[_]] = SList { type Head = T; type Tail[U] = _Tail[U] }

  object :: {
    def unapply[L <: SList](l: L): Option[(l.Head, l.Tail[l.Head])] =
      Some(
        (
          l.head,
          l.tail
        )
      )
  }

  case object `0` {
    def ::[T](h: T) = `1`(h)
    val size = 0
  }
  val   ⊥      = `0`
  type  ⊥      = `0`.type
  type `0`[T] = `0`.type
  case class `1`[T](head: T              ) extends SList { type Head = T; type Tail[U] = `0`[U]; def tail: `0`[T] = `0` }
  case class `2`[T](head: T, tail: `1`[T]) extends SList { type Head = T; type Tail[U] = `1`[U] }
  case class `3`[T](head: T, tail: `2`[T]) extends SList { type Head = T; type Tail[U] = `2`[U] }
  case class `4`[T](head: T, tail: `3`[T]) extends SList { type Head = T; type Tail[U] = `3`[U] }
  case class `5`[T](head: T, tail: `4`[T]) extends SList { type Head = T; type Tail[U] = `4`[U] }
  case class `6`[T](head: T, tail: `5`[T]) extends SList { type Head = T; type Tail[U] = `5`[U] }

  trait Cons[In[_]] {
    type Out[U] <: Aux[U, In]
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
  }

  implicit class Ops[T, Tail[_]](val tail: Tail[T]) extends AnyVal {
    def ::(head: T)(implicit cons: Cons[Tail]): cons.Out[T] = cons(head, tail)
    def size(implicit base: Base[Tail]): Int = base.size
  }

  trait FromList[F[_]] {
    val size: Int
    def apply[T](l: List[T]): Err | F[T]
  }
  object FromList {
    sealed trait Err
    case class TooFew [T](  gap:  Int   ) extends Err
    case class TooMany[T](extra: List[T]) extends Err
  }

  trait Base[F[_]]
    extends    Traverse[F]
       with Semigroupal[F]
       with   Scannable[F]
       with    FromList[F]

  trait Utils[F[_]]
    extends      Base[F]
       with Reducible[F]

  trait instances {
    import cats.implicits._

    def cons[Tail[_]](implicit tail: Base[Tail], ev: Cons[Tail]): Utils[ev.Out] = {
      type F[T] = ev.Out[T]
      val tl = tail
      new Utils[F] {
        override def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A ⇒ G[B]): G[F[B]] =
          fa match {
            case h :: t ⇒
              f(h)
                .map2(
                  tl.traverse(t)(f)
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
              (ha, hb) ::
              ta.product(tb)
          }

        override def scanLeft[A, B](fa: F[A], b: B, f: (B, A) ⇒ B): (F[B], B) = {
          val next = f(b, fa.head)
          val (scanned, total) = tl.scanLeft(fa.tail, next, f)
          (
            b :: scanned,
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
            subtotal :: tail
          )
        }

        val size = tail.size + 1
        // FromList
        override def apply[T](l: List[T]): Err | F[T] =
          l match {
            case Nil ⇒ L(TooFew(size))
            case scala.::(h, t) ⇒
              tail(t)
                .map {
                  t ⇒
                    h :: t
                }
          }
      }
    }

    implicit val utils0 =
      new Base[`0`] {
        type F[T] = `0`[T]
        val size = 0

        def traverse[G[_], A, B](fa: F[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[F[B]] = ev.pure(`0`)

        def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  =  b
        def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = lb

        def scanLeft [A, B](fa: F[A], b: B, f: (B, A) ⇒ B): (F[B],  B ) = (`0`, b)
        def scanRight[A, B](fa: F[A], b: B, f: (A, B) ⇒ B): (  B, F[B]) = (b, `0`)

        def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = `0`

        def apply[T](f: `0`[T]): List[T] = Nil
        def apply[T](l: List[T]): Err | F[T] =
          l match {
            case   Nil ⇒ R(`0`)
            case extra ⇒ L(TooMany(extra))
          }
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
