package org.lasersonlab.shapeless

trait Scannable[F[_]] {
  def scanLeft [A, B](fa: F[A], b: B, f: (B, A) ⇒ B): (F[B], B)
  def scanRight[A, B](fa: F[A], b: B, f: (A, B) ⇒ B): (B, F[B])
}
object Scannable {
  implicit class Ops[F[_], A](val fa: F[A]) extends AnyVal {
    @inline def scanLeft [B](b: B)(f: (B, A) ⇒ B)(implicit s: Scannable[F]): (F[B], B) = s.scanLeft (fa, b, f)
    @inline def scanRight[B](b: B)(f: (A, B) ⇒ B)(implicit s: Scannable[F]): (B, F[B]) = s.scanRight(fa, b, f)
  }

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
    }
}
