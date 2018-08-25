package org.lasersonlab.zarr.tlist

import hammerlab.shapeless.tlist._

trait ToList[TL <: TList] {
  type Elem
  def apply(tl: TL): List[Elem]
}
object ToList {
  type Aux[E, TL <: TList] = ToList[TL] { type Elem = E }
  implicit def tnil[E]: Aux[E, TNil] =
    new ToList[TNil] {
      type Elem = E
      def apply(tl: TNil): List[E] = Nil
    }

  implicit def cons[
    H,
    T <: TList
  ](
    implicit
    tail: Aux[H, T]
  ):
    Aux[H, H :: T] =
    new ToList[H :: T] {
      type Elem = H
      def apply(tl: H :: T): List[H] =
        tl match {
          case h :: t ⇒
            scala.::(h, tail(t))
        }
    }
}
