package org.lasersonlab.zarr.untyped

import cats.implicits._
import cats.{ Applicative, Eval, Foldable, Traverse }
import org.lasersonlab.ndarray.ArrayLike
import org.lasersonlab.shapeless.{ Scannable, Size, Zip }
import org.lasersonlab.zarr.{ Indices, UnfoldRange }
import org.lasersonlab.zarr.untyped.FlatArray.Idx
import Scannable.syntax._
import Zip.syntax._

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
    Indices.Aux[
      FlatArray[ShapeT, ?],
      ShapeT
    ] =
    new Indices[FlatArray[ShapeT, ?]] {
      type Shape[T] = ShapeT[T]
      @inline override def apply(shape: Shape[Idx]): FlatArray[Shape, Shape[Idx]] =
        FlatArray[Shape, Shape[Idx]](
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
