package org.lasersonlab.ndarray

/**
 * Type-class evidence that [[A]] is an N-dimensional array
 *
 * The `Shape` is a type-member, and methods for obtaining the shape, and randomly-accessing a specific element, are
 * provided
 */
trait ArrayLike[A[_]] {
  type Shape[_]
  def shape(a: A[_]): Shape[Int]
  def apply[T](a: A[T], idx: Shape[Int]): T
}

object ArrayLike {
  type Aux[A[_], S[_]] = ArrayLike[A] { type Shape[T] = S[T] }

  implicit class Ops[A[_], T](val a: A[T]) extends AnyVal {
    def apply[S[_]](idx: S[Int])(implicit ev: Aux[A, S]): T = ev(a, idx)
    def shape[S[_]](implicit ev: Aux[A, S]): S[Int] = ev.shape(a)
  }

  trait syntax {
    @inline implicit def makeArrayLikeOps[A[_], T](a: A[T]): Ops[A, T] = Ops(a)
  }
}
