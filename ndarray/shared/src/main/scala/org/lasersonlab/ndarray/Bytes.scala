package org.lasersonlab.ndarray

import java.nio.ByteBuffer

import cats.{ Eval, Foldable }
import org.lasersonlab.ndarray
import org.lasersonlab.ndarray.Arithmetic.Ops
import org.lasersonlab.ndarray.io.Read

trait Bytes[T] {
  type Shape

  def bytes: Seq[Byte]
  def shape: Shape
  def size: Int
  def sizeProducts: Shape

  implicit def arithmetic: Arithmetic.Id[Shape]
  implicit def read: Read[T]
  implicit def sum: Sum.Aux[Shape, Int]

  lazy val buff = ByteBuffer.wrap(bytes.toArray)

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
  type Aux[T, _Shape] = ndarray.Bytes[T] { type Shape = _Shape }

  case class Bytes[
    T,
    _Shape
  ](
    bytes: Seq[Byte],
    shape: _Shape,
    size: Int,
    sizeProducts: _Shape
  )(
    implicit
    val read: Read[T],
    val arithmetic: Arithmetic.Id[_Shape],
    val sum: Sum.Aux[_Shape, Int]
  )
  extends ndarray.Bytes[T] {
    type Shape = _Shape
  }

  implicit val foldable: Foldable[ndarray.Bytes] =
    new Foldable[ndarray.Bytes] {
      type F[A] = ndarray.Bytes[A]
      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B = {
        var ret = b
        var idx = 0
        while (idx < fa.size) {
          ret = f(ret, fa(idx))
          idx += 1
        }
        ret
      }
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
    }

  def foldableAux[Shape]: Foldable[Aux[?, Shape]] =
    new Foldable[Aux[?, Shape]] {
      type F[A] = Aux[A, Shape]
      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B = {
        var ret = b
        var idx = 0
        while (idx < fa.size) {
          ret = f(ret, fa(idx))
          idx += 1
        }
        ret
      }
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
    }

  implicit def arrayLike[S]: ArrayLike.Aux[Aux[?, S], S] =
    new ArrayLike[Aux[?, S]] {
      type Shape = S
      @inline def shape(a: Aux[_, S]): Shape = a.shape
      def apply[T](a: Aux[T, Shape], idx: Shape): T = a(idx)
    }
}
