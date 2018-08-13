package org.lasersonlab.zarr

import cats.Functor
import hammerlab.shapeless.tlist._

trait Indices[Shape, A[_]] {
  def apply(shape: Shape): A[Shape]
}

trait Empty[A[_]] {
  def apply[T](): A[T]
}

trait FromRows[A[_]] {
  type Row[_]
  def apply[T](rows: Seq[Row[T]]): A[T]
}
object FromRows {
  type Aux[A[_], _R[_]] = FromRows[A] { type Row[U] = _R[U] }
}

object Indices {

  implicit def tnil[A[_]](implicit empty: Empty[A]): Indices[TNil, A] =
    new Indices[TNil, A] {
      override def apply(shape: TNil): A[TNil] = empty()
    }

  implicit def cons[TL <: TList, A[_], Row[_]](
    implicit
    e: Indices[TL, Row],
    f: Functor[Row],
    pp: Prepend[Int, TL],
    fromRows: FromRows.Aux[A, Row]
  ):
    Indices[Int :: TL, A] =
    new Indices[Int :: TL, A] {
      def apply(shape: Int :: TL): A[Int :: TL] =
        shape match {
          case h :: t ⇒
            fromRows(
              (0 until h)
                .map {
                  i ⇒
                    f.map(
                      e(t)
                    ) {
                      i :: _
                    }
                }
            )
        }
    }
}
