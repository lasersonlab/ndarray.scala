package org.lasersonlab.ndarray

import lasersonlab.shapeless.slist._
import org.lasersonlab.ndarray.Vectors._

trait ArrayLike[A[_]] {
  type Shape[_]
  def shape(a: A[_]): Shape[Int]
  def apply[T](a: A[T], idx: Shape[Int]): T
}

object ArrayLike {
  type Aux[A[_], S[_]] = ArrayLike[A] { type Shape[T] = S[T] }

  implicit val vector1toArray: Aux[Vector1, `1`] =
    new ArrayLike[Vector1] {
      type Shape[T] = `1`[T]
      def shape(a: Vector1[_]): Shape[Int] = a.length :: ⊥
      def apply[T](a: Vector1[T], idx: Shape[Int]): T = a(idx.head)
    }

  def makeArrayLike[
    A[_],
    _Shape[_]
  ](
    implicit
    prev: Aux[A, _Shape],
    cons: Cons[_Shape]
  ):
    Aux[
      Vectors.Aux[?, A],
      cons.Out
    ] =
    new ArrayLike[Vectors.Aux[?, A]] {
      type Shape[T] = cons.Out[T]
      def shape(a: Vectors.Aux[_, A]): Shape[Int] =
        a.size ::
        prev.shape(
          // TODO: check sizes on Vectors construction, store Shape on Vectors
          a.rows.head
        )
      def apply[T](
        a: Vectors.Aux[T, A],
        idx: Shape[Int]
      ):
        T =
        prev(
          a(idx.head),
          idx.tail
        )
    }

  implicit val vector2toArray: Aux[Vector2, `2`] = makeArrayLike[Vector1, `1`]
  implicit val vector3toArray: Aux[Vector3, `3`] = makeArrayLike[Vector2, `2`]
  implicit val vector4toArray: Aux[Vector4, `4`] = makeArrayLike[Vector3, `3`]
  implicit val vector5toArray: Aux[Vector5, `5`] = makeArrayLike[Vector4, `4`]
  implicit val vector6toArray: Aux[Vector6, `6`] = makeArrayLike[Vector5, `5`]
}
