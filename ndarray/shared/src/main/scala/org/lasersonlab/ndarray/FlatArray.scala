package org.lasersonlab.ndarray

import cats.implicits._
import cats.{ Applicative, Eval, Foldable, Traverse }
import org.lasersonlab.ndarray.FlatArray.Idx
import org.lasersonlab.shapeless.Scannable.syntax._
import org.lasersonlab.shapeless.Zip.syntax._
import org.lasersonlab.shapeless.{ Scannable, Size, Zip }

case class FlatArray[
  ShapeT[_]
  : Foldable
  : Scannable
  : Size
  : Zip,
  T
](
  shape: ShapeT[Idx],
  elems: Vector[T]
) {
  val (size, strides) = shape.scanRight(1)(_ * _)
  val rank = Size(shape)
  def apply(idx: ShapeT[Idx]): T =
    elems(
      idx
        .zip(strides)
        .foldLeft(0) {
          case (sum, (idx,  stride)) ⇒
                sum + idx * stride
        }
    )
}

object FlatArray {
  type Idx = Int

  /** [[FlatArray]] with an unknown (at compile-time) number of dimensions */
  type *[T] = FlatArray[List, T]

  implicit def arrayLike[ShapeT[_]]: ArrayLike.Aux[FlatArray[ShapeT, ?], ShapeT] = {
    type F[T] = FlatArray[ShapeT, T]
    new ArrayLike[F] {
      type Shape[T] = ShapeT[T]
      @inline def shape(a: F[_]): Shape[Idx] = a.shape
      @inline def apply[T](a: F[T], idx: Shape[Idx]): T = a(idx)
    }
  }

  implicit def indices[
    ShapeT[_]
    : Foldable
    : Scannable
    : Size
    : Zip
  ](
    implicit u: UnfoldRange[ShapeT]
  ):
    Indices[
      FlatArray[ShapeT, ?],
      ShapeT
    ] =
    new Indices[FlatArray[ShapeT, ?], ShapeT] {
      @inline override def apply(shape: Shape): FlatArray[ShapeT, Index] =
        FlatArray[ShapeT, Index](
          shape,
          u(shape)
        )
    }

  implicit def traverse[
    ShapeT[_]
    : Foldable
    : Scannable
    : Size
    : Zip
  ]:
    Traverse[
      FlatArray[ShapeT, ?]
    ]
  = {
    type F[A] = FlatArray[ShapeT, A]
    new Traverse[F] {
      override def traverse[G[_], A, B](fa: F[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[F[B]] = {
        foldLeft(
          fa,
          ev.pure { Vector.newBuilder[B] }
        ) {
          (builder, elem) ⇒
            builder.map2(f(elem)) { _ += _ }
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
}
