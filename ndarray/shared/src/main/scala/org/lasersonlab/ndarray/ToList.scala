package org.lasersonlab.ndarray

trait ToList[T, TL <: TList] {
  def apply(tl: TL): List[T]
}
object ToList {
  implicit def tnil[T]: ToList[T, TNil.type] =
    new ToList[T, TNil.type] {
      def apply(tl: TNil.type): List[T] = Nil
    }

  implicit def cons[T, L <: TList.Aux[T]](implicit ev: ToList[T, L]): ToList[T, T :: L] =
    new ToList[T, T :: L] {
      def apply(tl: T ::L): List[T] = tl.head :: ev(tl.tail)
    }
}
