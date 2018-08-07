package org.lasersonlab.zarr

import org.hammerlab.shapeless.tlist._
import shapeless.Lazy

trait FromSeq[TL <: TList] {
  type Elem
  def apply(ts: Seq[Elem]): Either[String, TL]
}
object FromSeq {
  type Aux[E, TL <: TList] = FromSeq[TL] { type Elem = E }

  implicit def tnil[E]: Aux[E, TNil] =
    new FromSeq[TNil] {
      type Elem = E
      override def apply(ts: Seq[Elem]): Either[String, TNil] =
        if (ts.isEmpty)
          Right(TNil)
        else
          Left(
            s"Found extra elements: ${ts.mkString(",")}"
          )
    }

  implicit def cons[T, TL <: TList](
    implicit
    tl: Lazy[Aux[T, TL]],
    pp: Prepend[T, TL]
  ):
    Aux[T, T :: TL] =
    new FromSeq[T :: TL] {
      type Elem = T
      override def apply(ts: Seq[T]): Either[String, T :: TL] =
        if (ts.isEmpty)
          Left(
            "Too few elements"
          )
        else
          tl
            .value(ts.tail)
            .map {
              ts.head :: _
            }
    }
}

