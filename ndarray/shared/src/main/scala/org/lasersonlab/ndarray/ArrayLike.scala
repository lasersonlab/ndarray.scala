package org.lasersonlab.ndarray

import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.Ints._
import org.lasersonlab.ndarray.Vectors._

trait ArrayLike[A[_]] {
  type Shape
  def shape(a: A[_]): Shape
  def apply[T](a: A[T], idx: Shape): T
}

object ArrayLike {
  type Aux[A[_], S] = ArrayLike[A] { type Shape = S }

  implicit val vector1toArray: Aux[Vector1, Ints1] =
    new ArrayLike[Vector1] {
      type Shape = Ints1
      def shape(a: Vector1[_]): Shape = a.length :: TNil
      def apply[T](a: Vector1[T], idx: Shape): T = a(idx.head)
    }

  def makeArrayLike[
    A[_],
    _Shape <: TList
  ](
    implicit
    prev: Aux[A, _Shape],
    pp: Prepend[Int, _Shape]
  ):
    Aux[
      Vectors.Aux[?, A],
      Int :: _Shape
    ] =
    new ArrayLike[Vectors.Aux[?, A]] {
      type Shape = Int :: _Shape
      def shape(a: Vectors.Aux[_, A]): Shape =
        a.size ::
        prev.shape(
          // TODO: check sizes on Vectors construction, store Shape on Vectors
          a.rows.head
        )
      def apply[T](
        a: Vectors.Aux[T, A],
        idx: Shape
      ):
        T =
        prev(
          a(idx.head),
          idx.tail
        )
    }

  implicit val vector2toArray: Aux[Vector2, Ints2] = makeArrayLike[Vector1, Ints1]
  implicit val vector3toArray: Aux[Vector3, Ints3] = makeArrayLike[Vector2, Ints2]
  implicit val vector4toArray: Aux[Vector4, Ints4] = makeArrayLike[Vector3, Ints3]
  implicit val vector5toArray: Aux[Vector5, Ints5] = makeArrayLike[Vector4, Ints4]
  implicit val vector6toArray: Aux[Vector6, Ints6] = makeArrayLike[Vector5, Ints5]

  implicitly[Aux[Vector2, Ints2]]
  implicitly[Aux[Vector3, Ints3]]
}
