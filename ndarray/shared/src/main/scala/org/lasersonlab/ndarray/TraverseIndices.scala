package org.lasersonlab.ndarray

trait TraverseIndices[Shape <: TList] {
  def apply(shape: Shape): Iterator[Shape]
}

object TraverseIndices {
  implicit val tnil: TraverseIndices[TNil] = _ ⇒ Iterator()
  implicit def ints[TL <: TList.Aux[Int]](implicit ev: TraverseIndices[TL]): TraverseIndices[Int :: TL] =
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
