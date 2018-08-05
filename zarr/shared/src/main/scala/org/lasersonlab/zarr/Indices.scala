package org.lasersonlab.zarr

import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray
import Indices.Arr

trait Indices[Shape] {
  def apply(shape: Shape): Arr[Shape]
}

object Indices {
  type Arr[Shape] = ndarray.Array.Aux[Shape, Shape]
  implicit val tnil: Indices[TNil] =
    new Indices[TNil] {
      override def apply(shape: TNil): Arr[TNil] = ndarray.Array[TNil]()
    }

  implicit def cons[TL <: TList](
    implicit
    e: Indices[TL],
    pp: Prepend[Int, TL]
  ):
    Indices[Int :: TL] =
    new Indices[Int:: TL] {
      def apply(shape: Int:: TL): Arr[Int:: TL] =
        shape match {
          case h :: t ⇒
            ndarray.Array(
              (0 until h)
                .map {
                  i ⇒
                    e(t)
                      .map {
                        i :: _
                      }
                }
            )
        }
    }
}
