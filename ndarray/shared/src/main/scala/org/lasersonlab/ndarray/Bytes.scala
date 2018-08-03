package org.lasersonlab.ndarray

import java.nio.ByteBuffer

case class Bytes[
  T,
  Idx <: TList.Aux[Int]
](
  bytes: Array[Byte],
  shape: Idx
)(
  implicit
  read: Read[T],
  toList: ToList[Int, Idx]
) {
  val buff = ByteBuffer.wrap(bytes)
  val shapeList = toList(shape)
  val sizeProducts = shapeList.scanRight(1)(_ * _).drop(1)
  def apply(idx: Idx): T = {
    val offset =
      toList(idx)
        .iterator
        .zip(
          sizeProducts.iterator
        )
        .foldLeft(0) {
          case (idx, (offset, stepSize)) ⇒
            idx + offset * stepSize
        }
    read(buff, offset)
  }
}
