package org.lasersonlab.ndarray

import java.nio.ByteBuffer

case class Bytes[
  T,
  Shape
](
  bytes: scala.Array[Byte],
  shape: Shape
)(
  implicit
  read: Read[T],
  toList: ToList[Int, Shape]
) {
  val buff = ByteBuffer.wrap(bytes)
  val shapeList = toList(shape)
  val sizeProducts = shapeList.scanRight(1)(_ * _).drop(1)
  def apply(idx: Shape): T = {
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

object Bytes {
  case class Builder[T](bytes: scala.Array[Byte]) {
    def apply[Shape](shape: Shape)(
      implicit
      read: Read[T],
      toList: ToList[Int, Shape]
    ):
      Bytes[T, Shape] =
      Bytes[T, Shape](
        bytes,
        shape
      )
  }
  def apply[T](bytes: scala.Array[Byte]): Builder[T] = Builder(bytes)
}
