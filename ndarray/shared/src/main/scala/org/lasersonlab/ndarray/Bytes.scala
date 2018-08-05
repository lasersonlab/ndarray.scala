package org.lasersonlab.ndarray

import java.nio.ByteBuffer

import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray

//trait ScanRight[In, InElem, OutElem, Out] {
//  def apply(
//    in: In,
//    init: OutElem,
//    fn: (InElem, OutElem) ⇒ OutElem
//  ):
//    (
//      OutElem,
//      Out
//    )
//}

trait FoldLeft[T, Elem, Out] {
  def apply(t: T, init: Out, fn: (Out, Elem) ⇒ Out): Out
}

trait Sum[T] {
  type Out
  def apply(t: T): Out
}
object Sum {
  type Aux[T, _O] = Sum[T] { type Out = _O }
}

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
