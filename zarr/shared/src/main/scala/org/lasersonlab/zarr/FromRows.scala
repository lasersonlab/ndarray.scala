package org.lasersonlab.zarr

import cats.implicits._
import cats.{ Id, Traverse }
import org.lasersonlab.ndarray.Vectors
import shapeless.Lazy

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
    row: Lazy[Aux[Arr, Row]],
    traverse: Traverse[Arr]
  ):
    Aux[
      Vectors.Aux[?, Arr],
      Arr
    ] =
    new FromRows {
      type A[U] = Vectors.Aux[U, Arr]
      type Row[U] = Arr[U]
      def apply[T](rows: Seq[Arr[T]]): A[T] = Vectors.make[T, Arr](rows.toVector)
    }
}
