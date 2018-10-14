package org.lasersonlab.zarr.cmp.untyped

import cats.implicits._
import hammerlab.either._
import hammerlab.option._
import org.lasersonlab.zarr.FillValue.NonNull
import org.lasersonlab.zarr.array.metadata.?
import org.lasersonlab.zarr.cmp.Cmp
import org.lasersonlab.zarr.dtype._
import org.lasersonlab.zarr.{ Dimension, FillValue, Metadata, dtype }
import shapeless.the

object metadata {
  def cmpT[T](implicit t: Cmp[T]): Cmp[T] = t

  import dtype.{ DataType ⇒ dt }
  def cmpFromDatatype[T](d: DataType[T]): Cmp[T] =
    d match {
      case d @ dt.  byte      ⇒ cmpT
      case d @ dt. short  (_) ⇒ cmpT
      case d @ dt.   int  (_) ⇒ cmpT
      case d @ dt.  long  (_) ⇒ cmpT
      case d @ dt. float  (_) ⇒ cmpT
      case d @ dt.double  (_) ⇒ cmpT
      case d @ dt.string  (_) ⇒ cmpT
      case d @ dt.struct.?(_) ⇒ Cmp { (l, r) ⇒ (l != r) ? (l, r) }
      case d @ dt.struct  (_) ⇒ Cmp { (l, r) ⇒ (l != r) ? (l, r) }
    }

  implicit def fillValueCanEq[T](
    implicit
    d: DataType[T]
  ):
    Cmp[
      FillValue[T]
    ] =
    Cmp {
      case (NonNull(l), NonNull(r)) ⇒ cmpFromDatatype(d)(l, r).map(R(_))
      case (NonNull(l), r) ⇒ Some(L(L(l)))
      case (l, NonNull(r)) ⇒ Some(L(R(r)))
      case _ ⇒ None
    }

  object base {
    def cmp[
      Shape[_],
      Idx
    ](
      implicit
      dim: Cmp[Shape[Dimension[Idx]]]
    ):
      Cmp[
        ?[
          Shape,
          Idx
        ]
      ] = {
      Cmp[
        ?[
          Shape,
          Idx
        ],
        Any
      ] {
        (l, r) ⇒
          type T = r.T
          (l, r) match {
            case (
              l: Metadata[Shape, Idx, T],
              r: Metadata[Shape, Idx, T]
            ) ⇒
              implicit val d = l.dtype
              the[Cmp[Metadata[Shape, Idx, T]]].apply(l, r)
            case _ ⇒
              Some(s"Differing elem types: $l $r")
          }
      }
    }
  }
  trait cmp {
    implicit def baseCmp[
      Shape[_],
      Idx
    ](
      implicit
      dim: Cmp[Shape[Dimension[Idx]]]
    ):
      Cmp[
        ?[
          Shape,
          Idx
        ]
      ] =
      base.cmp
  }
  object cmp extends cmp
}
