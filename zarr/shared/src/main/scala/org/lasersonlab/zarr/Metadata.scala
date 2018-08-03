package org.lasersonlab.zarr

import hammerlab.option._
import Format._
//import org.lasersonlab.ndarray.TList
//import org.lasersonlab.zarr.Metadata.Shape

//case class Shape(dimensions: Seq[Int]) {
//  val product =
//    dimensions
//      .scanRight(1) { _ * _ }
//      .drop(1)
//}

case class Metadata[T, Shape](
  shape: Shape,
  chunks: Shape,
  dtype: DataType[T],
  compressor: Compressor,
  order: Order,
  fill_value: Opt[T] = None,
  zarr_format: Format = `2`,
  filters: Seq[Filter] = Nil
)

object Metadata {
//  type Shape = TList.Aux[Int]
}
