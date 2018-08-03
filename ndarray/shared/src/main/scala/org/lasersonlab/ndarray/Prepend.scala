package org.lasersonlab.ndarray

import org.lasersonlab.ndarray

trait Prepend[H, T <: TList] {
  def apply(h: H, t: T): H :: T// = ndarray.::(h, t)
}
object Prepend {
  implicit def tnil[H]: Prepend[H, TNil] =
    new Prepend[H, TNil] {
      def apply(h: H, t: TNil): H :: TNil = ndarray.::(h, t)
    }

  implicit def cons[H, L <: TList.Aux[H]]: Prepend[H, L] =
    new Prepend[H, L] {
      def apply(h: H, l: L): H :: L = ndarray.::(h, l)
    }
}
