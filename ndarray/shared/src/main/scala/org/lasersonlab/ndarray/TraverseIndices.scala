package org.lasersonlab.ndarray

trait TraverseIndices[Shape <: TList] {
  def apply(shape: Shape): Iterator[Shape]
}

object TraverseIndices {
  implicit val zero: TraverseIndices[TNil] = _ ⇒ Iterator()
  implicit val one: TraverseIndices[Int :: TNil] =
    new TraverseIndices[Int :: TNil] {
      def apply(shape: Int :: TNil): Iterator[Int :: TNil] =
        (0 until shape.head)
          .iterator
          .map(_ :: TNil)
    }
  implicit def recurse[TL <: TList.Aux[Int]](implicit ev: TraverseIndices[TL]): TraverseIndices[Int :: TL] =
    new TraverseIndices[Int :: TL] {
      override def apply(shape: Int :: TL): Iterator[Int :: TL] =
        shape match {
          case h :: t ⇒
            for {
              h ← (0 until h).iterator
              t ← ev(t)
            } yield
              h :: t
        }
    }
}
