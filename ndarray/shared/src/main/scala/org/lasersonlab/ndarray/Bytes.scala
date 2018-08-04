package org.lasersonlab.ndarray

import java.nio.ByteBuffer

import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray

abstract class Bytes[
  T,
  Shape
](
  val bytes: scala.Array[Byte],
  val shape: Shape
)(
  implicit
  read: Read[T],
  toList: ToList[Int, Shape]
) {
  val buff = ByteBuffer.wrap(bytes)
  val shapeList = toList(shape)
  import scala.::
  val total :: sizeProducts = shapeList.scanRight(1)(_ * _)
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
  case class Bytes[
    T,
    Shape
  ](
    override val bytes: scala.Array[Byte],
    override val shape: Shape
  )(
    implicit
    read: Read[T],
    toList: ToList[Int, Shape]
  )
  extends ndarray.Bytes[T, Shape](bytes, shape)


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
