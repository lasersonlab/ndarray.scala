package org.lasersonlab.zarr

import hammerlab.shapeless.tlist._

/**
 * Type-class implementing conversion of ND-array indices into string keys
 */
trait Key[T] {
  @inline def apply(t: T): String = builder(new StringBuilder, t).result()
  def builder(builder: StringBuilder, t: T): StringBuilder
}

object Key {
  def apply[T](t: T)(implicit key: Key[T]): String = key(t)

  implicit def seq[T]: Key[Seq[T]] =
    new Key[Seq[T]] {
      def builder(builder: StringBuilder, t: Seq[T]): StringBuilder =
        t match {
          case Seq() ⇒ builder
          case ts ⇒
            builder ++= ts.head.toString
            this.builder(builder, ts.tail)
        }
    }

  implicit def base[T]: Key[T :: TNil] =
    new Key[T :: TNil] {
      override def builder(builder: StringBuilder, t: T :: TNil): StringBuilder =
        builder ++= t.head.toString
    }

  implicit def cons[T, TL <: TList.Aux[T]](implicit tl: Key[TL]): Key[T :: TL] =
    new Key[T :: TL] {
      override def builder(builder: StringBuilder, t: T :: TL): StringBuilder = {
        builder ++= t.head.toString += '.'
        tl.builder(builder, t.tail)
      }
    }
}
