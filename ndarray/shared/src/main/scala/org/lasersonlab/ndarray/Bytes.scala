package org.lasersonlab.ndarray

import java.nio.ByteBuffer

import cats.Monoid
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray
import shapeless.Lazy

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

trait Sum[In] {
  type Out
  def apply(in: In): Out
}
object Sum {
  type Aux[T, _O] = Sum[T] { type Out = _O }
  implicit def tnil[T](implicit m: Monoid[T]): Aux[TNil, T] =
    new Sum[TNil] {
      type Out = T
      def apply(t: TNil): T = m.empty
    }

  implicit def cons[E, T <: TList](implicit p: Lazy[Aux[T, E]], m: Monoid[E], pp: Prepend[E, T]): Aux[E :: T, E] =
    new Sum[E :: T] {
      type Out = E
      def apply(in: E :: T): E = m.combine(in.head, p.value(in.tail))
    }
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
