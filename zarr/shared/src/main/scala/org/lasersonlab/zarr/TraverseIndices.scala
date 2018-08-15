package org.lasersonlab.zarr

import hammerlab.shapeless.tlist._

trait TraverseIndices[TL <: TList] {
  def apply(tl: TL): Iterator[TL]
}
object TraverseIndices {
  implicit val tnil: TraverseIndices[TNil] =
    new TraverseIndices[TNil] {
      override def apply(tl: TNil): Iterator[TNil] = Iterator(TNil)
    }

  implicit def cons[TL <: TList](
    implicit
    ti: TraverseIndices[TL],
    pp: Prepend[Int, TL]
  ):
    TraverseIndices[Int :: TL] =
    new TraverseIndices[Int :: TL] {
      def apply(tl: Int :: TL): Iterator[Int :: TL] =
        tl match {
          case h :: t ⇒
            for {
              r ← (0 until h).iterator
              rest ← ti(t)
            } yield
              r :: rest
        }
    }
}
