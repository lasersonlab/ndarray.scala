package org.lasersonlab.ndarray

import cats.implicits._
import cats.{ Applicative, Eval, Foldable, Traverse }
import org.lasersonlab.ndarray.Vector.Idx
import lasersonlab.shapeless.slist._
import org.lasersonlab.shapeless.Scannable.syntax._
import org.lasersonlab.shapeless.Zip.syntax._
import org.lasersonlab.shapeless.{ Scannable, Size, Zip }

case class Vector[
  ShapeT[_]
  : Traverse
  : Scannable
  : Size
  : Zip,
  T
](
  shape: ShapeT[Idx],
  elems: scala.Vector[T]
) {
  val (size, strides) = {
    val (size, strides) = shape.scanRight(1)(_ * _)
    (
      size,
      strides.zipAndIndex(shape)
    )
  }
  val rank = Size(shape)
  def apply(idx: ShapeT[Idx]): T =
    elems(
      idx
        .zip(strides)
        .foldLeft(0) {
          case (
            sum,
            (
              idx,
              (stride, max, i)
            )
          ) ⇒
            if (idx >= max || idx < 0)
              throw new IndexOutOfBoundsException(
                s"Index $idx >= maximum $max on dimension $i"
              )
            sum + idx * stride
        }
    )
}

object Vector {
  type Idx = Int

  /**
   * [[Vector]] with an unknown (at compile-time) number of dimensions, where "shape" and indices are represented as
   * [[List]]s
   */
  type *[T] = Vector[List, T]

  def apply[
    Shape[_]
    : Traverse
    : Scannable
    : Size
    : Zip,
    T
  ](
    shape: Shape[Idx],
    elems: T*
  ):
    Vector[Shape, T] = {
    val size = shape.foldLeft(1)(_ * _ )
    if (elems.size != size)
      throw new IllegalArgumentException(
        s"Expected $size elems (shape: $shape) but found ${elems.size}: ${elems.mkString(",")}"
      )

    Vector(
      shape,
      elems.toVector
    )
  }

  sealed abstract class Arg[Shape[_], T](val value: Vector[Shape, T])
  object Arg {
    implicit def fromEv[In, Shape[_], T](in: In)(implicit isarg: IsArg.Aux[In, Shape, T]): Arg[Shape, T] = new Arg[Shape, T](isarg(in)) {}
  }

  import lasersonlab.shapeless.{ slist ⇒ s }

  sealed trait IsArg[In] {
    type ShapeT[_]
    type T
    def apply(in: In): Vector[ShapeT, T]
  }
  trait LowPriIsArg {
    type Aux[In, _ShapeT[_], _T] = IsArg[In] { type ShapeT[U] = _ShapeT[U]; type T = _T }
    type Ax [In, _ShapeT[_]    ] = IsArg[In] { type ShapeT[U] = _ShapeT[U] }

    def make[In, S[_], _T](f: In ⇒ Vector[S, _T]): Aux[In, S, _T] =
      new IsArg[In] {
        type ShapeT[U] = S[U]
        type T = _T
        def apply(in: In): Vector[S, T] = f(in)
      }

    implicit def seq[S[U] <: Seq[U], T]: Aux[    S[T], s.`1`, T] = make { s ⇒ Vector(s.  size :: ⊥, s.toVector) }
    implicit def arr[                T]: Aux[Array[T], s.`1`, T] = make { s ⇒ Vector(s.length :: ⊥, s.toVector) }
  }

  case class RaggedInput[T](l: T, r: T) extends RuntimeException

  object IsArg
    extends LowPriIsArg {
    implicit def consseq[
      In,
      Tail[_],
      Out[U]
          <: SList.Aux[U, Tail]
           : Traverse
           : Scannable
           : Zip
           : Size
    ](
      implicit
      arg: IsArg.Ax[In, Tail],
      cons: Cons.Aux[Tail, Out]
    ):
      Aux[
        Seq[In],
        Out,
        arg.T
      ] =
      make[Seq[In], cons.Out, arg.T] {
        seq ⇒
          wrap(
            seq
              .map(arg(_))
              .toVector
          )
      }

    implicit def consarr[
      In,
      Tail[_],
      Out[U]
          <: SList.Aux[U, Tail]
           : Traverse
           : Scannable
           : Zip
           : Size
    ](
      implicit
      arg: IsArg.Ax[In, Tail],
      cons: Cons.Aux[Tail, Out]
    ):
      Aux[
        Array[In],
        Out,
        arg.T
      ] =
      make[Array[In], cons.Out, arg.T] {
        arr ⇒
          wrap(
            arr
              .map(arg(_))
              .toVector
          )
      }
  }

  implicit def wrap[
    Tail[_],
    T,
    Out[U]
        <: SList.Aux[U, Tail]
         : Traverse
         : Scannable
         : Zip
         : Size
  ](
    rows: scala.Vector[Vector[Tail, T]]
  )(
    implicit
    cons: Cons.Aux[Tail, Out]
  ):
    Vector[Out, T] = {
    val shape =
      rows
        .map(_.shape)
        .reduce {
          (l, r) ⇒
            if (l != r)
              throw RaggedInput(l, r)

            l
        }

    Vector(
      rows.length :: shape,
      rows.flatMap(_.elems)
    )
  }

  def conv[
    Tail[_],
    T,
    Out[U]
        <: SList.Aux[U, Tail]
         : Traverse
         : Scannable
         : Zip
         : Size
  ](
    rows: Arg[Tail, T]*
  )(
    implicit
    cons: Cons.Aux[Tail, Out]
  ):
    Vector[Out, T] =
    wrap(rows.map(_.value).toVector)

  implicit def arrayLike[ShapeT[_]]
    : ArrayLike.Aux[
      Vector[ShapeT, ?],
      ShapeT
    ]
  = {
    type F[T] = Vector[ShapeT, T]
    new ArrayLike[F] {
      type Shape[T] = ShapeT[T]
      @inline def shape   (a: F[_]):     Shape[Idx]     = a.shape
      @inline def apply[T](a: F[T], idx: Shape[Idx]): T = a(idx)
    }
  }

  implicit def indices[
    ShapeT[_]
    : Traverse
    : Scannable
    : Size
    : Zip
  ](
    implicit u: UnfoldRange[ShapeT]
  ):
    Indices[
      Vector[ShapeT, ?],
      ShapeT
    ] =
    new Indices[Vector[ShapeT, ?], ShapeT] {
      @inline override def apply(shape: Shape): Vector[ShapeT, Index] =
        Vector[ShapeT, Index](
          shape,
          u(shape)
        )
    }

  implicit def traverse[
    ShapeT[_]
    : Traverse
    : Scannable
    : Size
    : Zip
  ]:
    Traverse[
      Vector[ShapeT, ?]
    ]
  = {
    type F[A] = Vector[ShapeT, A]
    new Traverse[F] {
      override def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A ⇒ G[B]): G[F[B]] = {
        foldLeft(
          fa,
          scala.Vector.newBuilder[B].pure[G]
        ) {
          (builder, elem) ⇒
            builder.map2(f(elem)) { _ += _ }
        }
        .map {
          builder ⇒
            Vector(
              fa.shape,
              builder.result
            )
        }
      }
      @inline def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  = fa.elems.foldLeft ( b)(f)
      @inline def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.elems.foldRight(lb)(f)
    }
  }

  type `1`[T] = Vector[s.`1`, T]
  type `2`[T] = Vector[s.`2`, T]
  type `3`[T] = Vector[s.`3`, T]
  type `4`[T] = Vector[s.`4`, T]
  type `5`[T] = Vector[s.`5`, T]
  type `6`[T] = Vector[s.`6`, T]
  type `7`[T] = Vector[s.`7`, T]
  type `8`[T] = Vector[s.`8`, T]
  type `9`[T] = Vector[s.`9`, T]
}
