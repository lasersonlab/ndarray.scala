package org.lasersonlab.cats

import cats.{ Eval, Foldable }

object FoldArray
  extends Foldable[Array] {
  type F[A] = Array[A]
  @inline override def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  = fa.foldLeft(b)(f)
  @inline override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.foldRight(lb)(f)
  implicit val _instance = this
}
