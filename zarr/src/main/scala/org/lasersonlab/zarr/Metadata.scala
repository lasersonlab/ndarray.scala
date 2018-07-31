package org.lasersonlab.zarr

import hammerlab.option._
import Format._

case class Shape(dimensions: Seq[Int]) {
  val product =
    dimensions
      .scanRight(1) { _ * _ }
      .drop(1)
}

case class Metadata[T](
                          shape: Shape,
                          chunks: ChunkSize,
                          dtype: DataType[T],
                          compressor: Compressor,
                          order: Order,
                          fill_value: Opt[T] = None,
                          zarr_format: Format = `2`,
                          filters: Seq[Filter] = Nil
)
