package org.lasersonlab.zarr.cmp.untyped

import hammerlab.option._
import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.{ Dimension, untyped }

object metadata {
  def cmpT[T](implicit cmp: Cmp[T]): Cmp.Aux[T, (T, T)] =
    new Cmp[T] {
      type Diff = (T, T)
      def cmp(l: T, r: T): Option[Diff] = cmp(l, r).map(_ ⇒ (l, r))
    }

  def cmpFromDatatype(d: DataType): Cmp.Aux[d.T, (d.T, d.T)] =
    (
      d match {
        case d: DataType.Aux[untyped.Struct] ⇒ cmpT[untyped.Struct]
        case d: DataType.Aux[  Char] ⇒ cmpT[  Char]
        case d: DataType.Aux[  Byte] ⇒ cmpT[  Byte]
        case d: DataType.Aux[ Short] ⇒ cmpT[ Short]
        case d: DataType.Aux[   Int] ⇒ cmpT[   Int]
        case d: DataType.Aux[  Long] ⇒ cmpT[  Long]
        case d: DataType.Aux[ Float] ⇒ cmpT[ Float]
        case d: DataType.Aux[Double] ⇒ cmpT[Double]
        case d: DataType.Aux[String] ⇒ cmpT[String]
        case d: Struct[_, _] ⇒
          new Cmp[d.T] {
            type Diff = (d.T, d.T)
            def cmp(l: d.T, r: d.T): Option[(d.T, d.T)] = (l != r) ? (l, r)
          }
      }
    )
    .asInstanceOf[
      Cmp.Aux[
        d.T,
        (
          d.T,
          d.T
        )
      ]
    ]

  trait cmp {
    implicit def baseCmp[Shape[_], Idx](implicit dim: Cmp[Shape[Dimension[Idx]]]): Cmp[untyped.Metadata.S[Shape, Idx]] = {
      Cmp.by {
        m ⇒
          import shapeless._
          m.shape ::
          m.dtype ::
          m.compressor ::
          m.order ::
          // TODO: fill_value
          m.zarr_format ::
          m.filters ::
          HNil
      }
    }
  }
  object cmp extends cmp
}
