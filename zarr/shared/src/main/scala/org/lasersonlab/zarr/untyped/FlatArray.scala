package org.lasersonlab.zarr.untyped

import cats.implicits._
import cats.{ Applicative, Eval, Traverse }
import org.lasersonlab.ndarray.ArrayLike
import org.lasersonlab.zarr.Indices

case class FlatArray[T](shape: List[Int], elems: Seq[T]) {
  val size :: strides = shape.scanRight(1)(_ * _).toList
  val rank = shape.size
  def apply(idx: Seq[Int]): T = {
    var sum = 0
    val idxs = idx.iterator
    var stride = strides
    while (stride != Nil) {
      sum += idxs.next * stride.head
      stride = stride.tail
    }
    if (idxs.hasNext)
      throw new IllegalArgumentException(
        s"Got index of rank ${idx.size} (${idx.mkString(",")}) in array of rank $rank"
      )
    elems(sum)
  }
}

object FlatArray {
  implicit val arrayLike: ArrayLike.Aux[FlatArray, List[Int]] =
    new ArrayLike[FlatArray] {
      type Shape = List[Int]
      @inline def shape(a: FlatArray[_]): Shape = a.shape
      @inline def apply[T](a: FlatArray[T], idx: Shape): T = a(idx)
    }

  implicit val list: Indices.Aux[FlatArray, List[Int]] =
    new Indices[FlatArray] {
      type Shape = List[Int]
      @inline def apply(shape: Shape): FlatArray[Shape] =
        FlatArray(
          shape,
          rec(
            shape.toList
          )
        )
      private def rec(shape: List[Int]): List[List[Int]] =
        shape match {
          case Nil ⇒ Nil
          case scala.::(h, t) ⇒
            for {
              h ← (0 until h).toList
              t ← rec(t)
            } yield
              scala.::(h, t)
        }
    }

  implicit val traverse: Traverse[FlatArray] =
    new Traverse[FlatArray] {
      type F[A] = FlatArray[A]
      override def traverse[G[_], A, B](fa: F[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[F[B]] = {
        foldLeft(
          fa,
          ev.pure {
            Vector.newBuilder[B]
          }
        ) {
          (builder, elem) ⇒
            ev.map2(builder, f(elem)) { _ += _ }
        }
        .map {
          builder ⇒
            FlatArray(
              fa.shape,
              builder.result
            )
        }
      }
      @inline def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  = fa.elems.foldLeft ( b)(f)
      @inline def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.elems.foldRight(lb)(f)
    }
}
