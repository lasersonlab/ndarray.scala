package org.lasersonlab.zarr.cmp.untyped

import cats.implicits._
import hammerlab.either._
import hammerlab.option._
import org.lasersonlab.zarr.FillValue.NonNull
import org.lasersonlab.zarr.array.metadata.untyped.Shaped
import org.lasersonlab.zarr.cmp.Cmp
import org.lasersonlab.zarr.dtype._
import org.lasersonlab.zarr.{ Dimension, FillValue, Metadata, dtype }
import shapeless.the

object metadata {
  def cmpT[T](implicit t: Cmp[T]): Cmp[T] = t

  import dtype.{ DataType ⇒ dt }
  def cmpFromDatatype[T](d: DataType.Aux[T]): Cmp[T] =
    (
      d match {
        case d @ dt.  byte      ⇒ cmpT[  Byte]
        case d @ dt. short  (_) ⇒ cmpT[ Short]
        case d @ dt.   int  (_) ⇒ cmpT[   Int]
        case d @ dt.  long  (_) ⇒ cmpT[  Long]
        case d @ dt. float  (_) ⇒ cmpT[ Float]
        case d @ dt.double  (_) ⇒ cmpT[Double]
        case d @ dt.string  (_) ⇒ cmpT[String]
        case d @ dt.struct.?(_) ⇒ cmpT[dt.struct.?]
        case d @ dt.struct  (_) ⇒
          Cmp[d.T, (d.T, d.T)] {
            (l, r) ⇒ (l != r) ? (l, r)
          }
      }
    )
    .asInstanceOf[Cmp[d.T]]

  implicit def fillValueCanEq[T](
    implicit
    d: DataType.Aux[T]
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
        Shaped[
          Shape,
          Idx
        ]
      ] = {
      Cmp[
        Shaped[
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
        Shaped[
          Shape,
          Idx
        ]
      ] =
      base.cmp
  }
  object cmp extends cmp
}
