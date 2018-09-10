package org.lasersonlab.ndarray.io

import java.io.{ ByteArrayOutputStream, DataOutputStream }
import java.nio.ByteBuffer

import cats.{ Foldable, Functor }
import cats.implicits._

/**
 * Type-class for writing elements to a [[DataOutputStream]]
 */
trait Write[T] {
//  def apply(t: T): Array[Byte] = {
//    val baos = new ByteArrayOutputStream()
//    val data = new DataOutputStream(baos)
//    apply(data, t)
//    data.close()
//    baos.toByteArray
//  }
  def apply(os: ByteBuffer, t: T): Unit
}
trait LowPriWrite {
  implicit def functor[T[_] : Functor, E](
    implicit
    write: Write[E]
  ):
    Write[T[E]] =
    new Write[T[E]] {
      def apply(buffer: ByteBuffer, t: T[E]): Unit =
        t.map {
          write(buffer, _)
        }
    }
}
object Write
  extends LowPriWrite {
  implicit val int: Write[Int] =
    new Write[Int] {
      def apply(os: ByteBuffer, t: Int): Unit = os.putInt(t)
    }

  implicit def foldable[T[_] : Foldable, E](
    implicit
    write: Write[E]
  ):
    Write[T[E]] =
    new Write[T[E]] {
      def apply(buffer: ByteBuffer, t: T[E]): Unit =
        t.foldLeft(
          ()
        )(
          (_, elem) ⇒ write(buffer, elem)
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
//      override def apply(t: T[E]): Array[Byte] = {
//        val baos = new ByteArrayOutputStream()
//        val data = new DataOutputStream(baos)
//        functor.map(
//          t
//        )(
//          elem(data, _)
//        )
//        data.close()
//        baos.toByteArray
//      }
      override def apply(buffer: ByteBuffer, t: T[E]): Unit =
        functor.map(
          t
        )(
          elem(buffer, _)
        )
    }

  implicit class Ops[T](val t: T) extends AnyVal {
//    def write(implicit ev: Write[T]): Array[Byte] = ev(t)
    def write(os: ByteBuffer)(implicit ev: Write[T]): Unit = ev(os, t)
  }
}
