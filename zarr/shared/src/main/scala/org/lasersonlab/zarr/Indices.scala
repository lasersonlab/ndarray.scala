package org.lasersonlab.zarr

import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray
import Indices.Arr
import cats.Functor
import org.lasersonlab.ndarray.Array.Aux

trait Indices[Shape, A <: Arr[Shape]] {
  def apply(shape: Shape): A
}

object Indices {

  type Arr[Shape] = ndarray.Array.Aux[Shape, Shape]

  implicit val tnil: Indices[TNil, Arr[TNil]] =
    new Indices[TNil, Arr[TNil]] {
      override def apply(shape: TNil): Arr[TNil] = ndarray.Array[TNil]()
    }

  implicit def cons[TL <: TList](
    implicit
    e: Indices[TL, Arr[TL]],
    f: Functor[Aux[?, TL]],
    pp: Prepend[Int, TL]
  ):
    Indices[Int :: TL, Arr[Int :: TL]] =
    new Indices[Int:: TL, Arr[Int :: TL]] {
      def apply(shape: Int:: TL): Arr[Int:: TL] =
        shape match {
          case h :: t ⇒
            ndarray.Array(
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
