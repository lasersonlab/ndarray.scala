package org.lasersonlab.zarr

import cats.implicits._
import cats.{ Id, Traverse }
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.Vectors
import org.lasersonlab.ndarray.Vectors.{ Vector1, Vector2 }
import shapeless.Lazy
import hammerlab.shapeless

trait Test {
  type A[_]
  type B
}
object Test {
  type Aux[_A[_], _B] = Test { type A[U] = _A[U]; type B = _B }
  type WithA[_A[_]] = Test { type A[U] = _A[U] }
  type WithB[_B] = Test { type B = _B }

  implicit def cons[A[_]](
    implicit
    withA: Lazy[WithA[A]]
  ):
  Aux[
    withA.value.A,
    A[Int]
  ] = ???

//  implicitly[WithA[Id]]
//  implicitly[Lazy[WithA[Id]]]

}

trait FromRows {
  type A[_]
  type Row[_]
  def apply[T](rows: Seq[Row[T]]): A[T]
}
object FromRows {
  type Aux[_A[_], R[_]] = FromRows { type A[U] = _A[U]; type Row[U] = R[U] }
  type A[_A[_]] = FromRows { type   A[U] = _A[U] }
  type R[ R[_]] = FromRows { type Row[U] =  R[U] }

  implicit val fromId: Aux[Vectors.Aux[?, Id], Id] =
    new FromRows {
      type A[U] = Vectors.Aux[U, Id]
      type Row[U] = Id[U]
      def apply[T](rows: Seq[Id[T]]): A[T] = Vectors.make[T, Id](rows.toVector)
    }

  implicit def cons[Arr[_], Row[_]](
    implicit
    row: Lazy[Aux[Arr, Row]]
    //traverse: Traverse[Arr]
  ):
    Aux[
      Vectors.Aux[?, Arr],
      Arr
    ] =
    new FromRows {
      type A[U] = Vectors.Aux[U, Arr]
      type Row[U] = Arr[U]
      def apply[T](rows: Seq[Arr[T]]): A[T] = ??? //Vectors.make[T, Arr](rows.toVector)
    }
}

/**
 * Iterate over an N-dimensional range of integers (provided as a `Shape`), stored as an [[A]]
 */
trait Indices[A[_]] {
  type Shape
  def apply(shape: Shape): A[Shape]
}
object Indices {

  type Aux[A[_], _Shape] = Indices[A] { type Shape = _Shape }

  implicit val tnil: Aux[Id, TNil] =
    new Indices[Id] {
      type Shape = TNil
      def apply(tnil: TNil): Id[TNil] = tnil
    }

  // NOTE: the compiler doesn't use this to derive instances for `Vectors` types
  implicit def cons[TL <: TList, Row[_]](
    implicit
    e: Lazy[Aux[Row, TL]],
    f: Traverse[Row],
    pp: Prepend[Int, TL]
  ):
  Aux[
    Vectors.Aux[?, Row],
    Int :: TL
  ] =
    new Indices[Vectors.Aux[?, Row]] {
      type Shape = Int :: TL
      def apply(shape: Shape): Vectors.Aux[Shape, Row] =
        shape match {
          case h :: t ⇒
            Vectors.make[Shape, Row](
              (0 until h)
                .map {
                  i ⇒
                    f.map(
                      e.value(t)
                    ) {
                      i :: _
                    }
                }
                .toVector
            )
        }
    }

  // This crashes the compiler if it is searched for via `cons` above
  implicit val lazyIndices1: Lazy[Indices.Aux[Vector1,       Int :: TNil]] = Lazy(indices1)

  import hammerlab.collection._

  implicit val     indices1:      Indices.Aux[Vector1,       Int :: TNil]  = //Indices.cons[      TNil,      Id]
  new Indices[Vector1] {
    type Shape = Int :: TNil
    def apply(shape: Shape): Vector1[Shape] =
      shape match {
        case n :: TNil ⇒
          (0 until n).map[Shape, Vector[Shape]]{ _ :: TNil }
      }
  }
  implicit val     indices2:      Indices.Aux[Vector2, Int:: Int :: TNil]  = Indices.cons[Int :: TNil, Vector1]




//  implicit def cons[TL <: TList, Row[_]](
//    implicit
//    e: Lazy[Aux[Row, TL]],
//    f: Functor[Row],
//    pp: Prepend[Int, TL],
//    fromRows: FromRows.R[Row]
//  ):
//    Aux[
//      fromRows.A,
//      Int :: TL
//    ] =
//    new Indices[fromRows.A] {
//      type Shape = Int :: TL
//      def apply(shape: Shape): fromRows.A[Shape] =
//        shape match {
//          case h :: t ⇒
//            fromRows(
//              (0 until h)
//                .map {
//                  i ⇒
//                    f.map(
//                      e.value(t)
//                    ) {
//                      i :: _
//                    }
//                }
//            )
//        }
//    }
}
