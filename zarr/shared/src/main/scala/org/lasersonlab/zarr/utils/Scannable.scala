package org.lasersonlab.zarr.utils

trait Scannable[F[_]] {
  def scanLeft [A, B](fa: F[A], b: B, f: (B, A) ⇒ B): (F[B], B)
  def scanRight[A, B](fa: F[A], b: B, f: (A, B) ⇒ B): (B, F[B])
}
object Scannable {
  implicit class Ops[F[_], A](val fa: F[A]) extends AnyVal {
    @inline def scanLeft [B](b: B)(f: (B, A) ⇒ B)(implicit s: Scannable[F]): (F[B], B) = s.scanLeft (fa, b, f)
    @inline def scanRight[B](b: B)(f: (A, B) ⇒ B)(implicit s: Scannable[F]): (B, F[B]) = s.scanRight(fa, b, f)
  }
}
