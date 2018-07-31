package org.lasersonlab.zarr

import hammerlab.option._

//case class NDArray(shape: Shape, data)

case class Chunk[T](
  data: Array[Byte]
)(
  implicit
  dtype: DataType[T]
) {
  def apply(idx: List[Int], pos: Int = 0): T =
    idx match {
      case Nil ⇒ dtype(data, pos)
      case h :: t ⇒ ???
    }
}

case class Array[T](
  metadata: Metadata[T],
  data: Array[Byte],
  attrs: Opt[Attrs] = None
) {
  def apply(idx: List[Int], pos: Int = 0): Chunk[T] =
    idx match {
      case Nil ⇒ ???
      case h :: t ⇒ ???
    }
}

object Array {
}
