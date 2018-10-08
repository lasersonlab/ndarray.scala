package org.lasersonlab.ndarray

trait ArrayLike[A[_]] {
  type Shape[_]
  def shape(a: A[_]): Shape[Int]
  def apply[T](a: A[T], idx: Shape[Int]): T
}

object ArrayLike {
  type Aux[A[_], S[_]] = ArrayLike[A] { type Shape[T] = S[T] }
}
