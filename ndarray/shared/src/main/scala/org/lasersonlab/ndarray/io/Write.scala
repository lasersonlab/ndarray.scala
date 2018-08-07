package org.lasersonlab.ndarray.io

import java.io.{ ByteArrayOutputStream, DataOutputStream }
import java.nio.ByteBuffer

import cats.Functor

/**
 * Type-class for writing elements to a [[DataOutputStream]]
 */
trait Write[T] {
  def apply(t: T): Array[Byte]
  def apply(os: DataOutputStream, t: T): Unit
}
object Write {
  implicit val int: Write[Int] =
    new Write[Int] {
      def apply(t: Int): Array[Byte] =
        // TODO: endianness
        ByteBuffer
          .allocate(4)
          .putInt(t)
          .array()

      override def apply(os: DataOutputStream, t: Int): Unit = os.writeInt(t)
    }

  implicit def traverse[T[_], E](
    implicit
    functor: Functor[T],
    elem: Write[E]
  ):
    Write[T[E]] =
    new Write[T[E]] {
      def apply(t: T[E]): Array[Byte] = {
        val baos = new ByteArrayOutputStream()
        val data = new DataOutputStream(baos)
        functor.map(
          t
        )(
          elem(data, _)
        )
        data.close()
        baos.toByteArray
      }
      override def apply(os: DataOutputStream, t: T[E]): Unit =
        functor.map(
          t
        )(
          elem(os, _)
        )
    }

  import org.lasersonlab.ndarray.Array.{ Aux ⇒ Arr }
  implicit def array[E, Shape, T[U] <: Arr[U, Shape]](
    implicit
    functor: Functor[T],
    elem: Write[E]
  ):
    Write[T[E]] =
    new Write[T[E]] {
      override def apply(t: T[E]): Array[Byte] = {
        val baos = new ByteArrayOutputStream()
        val data = new DataOutputStream(baos)
        functor.map(
          t
        )(
          elem(data, _)
        )
        data.close()
        baos.toByteArray
      }
      override def apply(os: DataOutputStream, t: T[E]): Unit =
        functor.map(
          t
        )(
          elem(os, _)
        )
    }

  implicit class Ops[T](val t: T) extends AnyVal {
    def write(implicit ev: Write[T]): Unit = ev(t)
    def write(os: DataOutputStream)(implicit ev: Write[T]): Unit = ev(os, t)
  }
}
