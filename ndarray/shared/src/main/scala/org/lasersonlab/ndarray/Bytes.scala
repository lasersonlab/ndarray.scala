package org.lasersonlab.ndarray

import java.nio.ByteBuffer

import org.lasersonlab.ndarray
import org.lasersonlab.ndarray.io.Read

abstract class Bytes[
  T,
  Shape: Arithmetic.Id
](
  val bytes: scala.Array[Byte],
  val shape: Shape
)(
  implicit
  read: Read[T],
  val scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
  sum: Sum.Aux[Shape, Int],
) {
  import Arithmetic.Ops

  val buff = ByteBuffer.wrap(bytes)

  val (total, sizeProducts) = scanRight(shape, 1, _ * _)

  def apply(idx: Shape): T =
    read(
      buff,
      sum(
        idx * sizeProducts
      )
    )
}

object Bytes {
  case class Bytes[
    T,
    Shape: Arithmetic.Id
  ](
    override val bytes: scala.Array[Byte],
    override val shape: Shape
  )(
    implicit
    read: Read[T],
    scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
    sum: Sum.Aux[Shape, Int]
  )
  extends ndarray.Bytes[T, Shape](bytes, shape)

  case class Builder[T](bytes: scala.Array[Byte]) {
    def apply[
      Shape: Arithmetic.Id
    ](
      shape: Shape
    )(
      implicit
      read: Read[T],
      scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
      sum: Sum.Aux[Shape, Int]
    ):
      Bytes[T, Shape] =
      Bytes[T, Shape](
        bytes,
        shape
      )
  }
  def apply[T](bytes: scala.Array[Byte]): Builder[T] = Builder(bytes)
}
