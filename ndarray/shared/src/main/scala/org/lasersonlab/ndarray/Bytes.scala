package org.lasersonlab.ndarray

import java.nio.ByteBuffer

import org.lasersonlab.ndarray
import org.lasersonlab.ndarray.io.Read

abstract class Bytes[
  T,
  Shape: Arithmetic.Id
](
  val bytes: Seq[Byte],
  val shape: Shape,
  val size: Int,
  val sizeProducts: Shape
)(
  implicit
  read: Read[T],
//  val scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
  sum: Sum.Aux[Shape, Int],
) {
  import Arithmetic.Ops

  val buff = ByteBuffer.wrap(bytes.toArray)

//  val (size, sizeProducts) = scanRight(shape, 1, _ * _)

  @inline def apply(idx: Int): T = read(buff, idx)

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
    override val bytes: Seq[Byte],
    override val shape: Shape,
    override val size: Int,
    override val sizeProducts: Shape
  )(
    implicit
    read: Read[T],
    sum: Sum.Aux[Shape, Int]
  )
  extends ndarray.Bytes[T, Shape](bytes, shape, size, sizeProducts)

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
      Bytes[T, Shape] = {
      val (size, sizeProducts) = scanRight(shape, 1, _ * _)
      Bytes[T, Shape](
        bytes,
        shape,
        size,
        sizeProducts
      )
    }
  }

  def apply[T](bytes: scala.Array[Byte]): Builder[T] = Builder(bytes)
}
