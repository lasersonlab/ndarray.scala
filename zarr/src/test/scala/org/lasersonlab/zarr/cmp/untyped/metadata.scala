package org.lasersonlab.zarr.cmp.untyped

import cats.Traverse
import cats.implicits._
import hammerlab.either._
import hammerlab.option._
import org.lasersonlab.zarr.FillValue.NonNull
import org.lasersonlab.zarr.array.{ metadata ⇒ md }
import org.lasersonlab.zarr.cmp.Cmp
import org.lasersonlab.zarr.dtype._
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.zarr.{ Dimension, FillValue, Metadata, dtype }
import shapeless.the

object metadata {
  import dtype.{ DataType ⇒ dt }
  def cmpFromDatatype[T](d: DataType[T]): Cmp[T] =
    d match {
      case d @ dt.   I8       ⇒ the[Cmp[T]]
      case d @ dt.   I16  (_) ⇒ the[Cmp[T]]
      case d @ dt.   I32  (_) ⇒ the[Cmp[T]]
      case d @ dt.   I64  (_) ⇒ the[Cmp[T]]
      case d @ dt.   F32  (_) ⇒ the[Cmp[T]]
      case d @ dt.   F64  (_) ⇒ the[Cmp[T]]
      case d @ dt.string  (_) ⇒ the[Cmp[T]]
      case d @ dt.struct.?(_) ⇒ Cmp { (l, r) ⇒ (l != r) ? (l, r) }
      case d @ dt.struct  (_) ⇒ Cmp { (l, r) ⇒ (l != r) ? (l, r) }
    }

  implicit def fillValueCmp[T](
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

  trait cmp {
    def baseCmp[
      Shape[_]: Traverse,
        Idx   : Idx.T
    ](
      implicit
      dim: Cmp[Shape[Dimension[Idx]]]
    ):
      Cmp[
        md.?[
          Shape,
          Idx
        ]
      ] = {
      Cmp[
        md.?[
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
  object cmp extends cmp
}
