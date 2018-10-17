package org.lasersonlab.slist

trait Scannable[F[_]] {
  def scanLeft   [A, B](fa: F[A], b: B, f: (B, A) ⇒ B): (F[B],  B )
  def scanLeft_→ [A, B](fa: F[A], b: B, f: (B, A) ⇒ B):  F[B]
  def scanRight  [A, B](fa: F[A], b: B, f: (A, B) ⇒ B): (  B, F[B])
  def scanRight_←[A, B](fa: F[A], b: B, f: (A, B) ⇒ B):       F[B]
}
object Scannable {
  implicit class Ops[F[_], A](val fa: F[A]) extends AnyVal {
    @inline def scanLeft   [B](b: B)(f: (B, A) ⇒ B)(implicit s: Scannable[F]): (F[B], B) = s.scanLeft   (fa, b, f)
    @inline def scanLeft_→ [B](b: B)(f: (B, A) ⇒ B)(implicit s: Scannable[F]):  F[B]     = s.scanLeft_→ (fa, b, f)
    @inline def scanRight  [B](b: B)(f: (A, B) ⇒ B)(implicit s: Scannable[F]): (B, F[B]) = s.scanRight  (fa, b, f)
    @inline def scanRight_←[B](b: B)(f: (A, B) ⇒ B)(implicit s: Scannable[F]):     F[B]  = s.scanRight_←(fa, b, f)
  }

  trait syntax {
    @inline implicit def slistScannableOps[F[_], A](fa: F[A]): Ops[F, A] = Ops(fa)
  }
  object syntax extends syntax

  implicit val list: Scannable[List] =
    new Scannable[List] {
      override def scanLeft[A, B](fa: List[A], b: B, f: (B, A) ⇒ B): (List[B], B) =
        fa match {
          case Nil ⇒ (Nil, b)
          case h :: t ⇒
            val next = f(b, h)
            val (tail, total) = scanLeft(t, next, f)
            (
              b :: tail,
              total
            )
        }

      override def scanLeft_→[A, B](fa: List[A], b: B, f: (B, A) ⇒ B): List[B] =
        fa match {
          case Nil ⇒ Nil
          case h :: t ⇒
            val next = f(b, h)
            next :: scanLeft_→(t, next, f)
        }

      override def scanRight[A, B](fa: List[A], b: B, f: (A, B) ⇒ B): (B, List[B]) =
        fa match {
          case Nil ⇒ (b, Nil)
          case h :: t ⇒
            val (subtotal, tail) = scanRight(t, b, f)
            (
              f(h, subtotal),
              subtotal :: tail
            )
        }

      override def scanRight_←[A, B](fa: List[A], b: B, f: (A, B) ⇒ B): List[B] =
        fa match {
          case Nil ⇒ Nil
          case h :: t ⇒
            val tail = scanRight_←(t, b, f)
            f(h, b) :: tail
        }
    }
}
