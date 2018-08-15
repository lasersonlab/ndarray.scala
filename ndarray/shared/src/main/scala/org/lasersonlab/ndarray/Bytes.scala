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
  sum: Sum.Aux[Shape, Int],
) {
  import Arithmetic.Ops

  val buff = ByteBuffer.wrap(bytes.toArray)

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
}
