package org.lasersonlab.ndarray

trait ToList[T, TL] {
  def apply(tl: TL): List[T]
}
object ToList {
  implicit def tnil[T]: ToList[T, TNil] =
    new ToList[T, TNil] {
      def apply(tl: TNil): List[T] = Nil
    }

  implicit def cons[T, L <: TList](implicit ev: ToList[T, L]): ToList[T, T :: L] =
    new ToList[T, T :: L] {
      def apply(tl: T ::L): List[T] = tl.head :: ev(tl.tail)
    }
}
