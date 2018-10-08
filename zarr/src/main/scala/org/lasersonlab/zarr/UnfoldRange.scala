package org.lasersonlab.zarr

trait UnfoldRange[Shape[_]] {
  def apply(s: Shape[Int]): Vector[Shape[Int]]
}
object UnfoldRange {
  implicit val list: UnfoldRange[List] =
    new UnfoldRange[List] {
      override def apply(s: List[Int]): Vector[List[Int]] =
        s match {
          case Nil ⇒ Vector(Nil)
          case h :: t ⇒
            for {
              h ← (0 until h).toVector
              t ← this(t)
            } yield
              h :: t
        }
    }

  import lasersonlab.shapeless.slist._

  implicit val _0: UnfoldRange[`0`] = new UnfoldRange[`0`] {
    def apply(s: `0`[Int]): Vector[`0`[Int]] = Vector()
  }
  implicit def cons[Tail[_]](
    implicit
    cons: Cons[Tail],
    tail: UnfoldRange[Tail]
  ):
    UnfoldRange[cons.Out] = {
    type Out = cons.Out[Int]
    new UnfoldRange[cons.Out] {
      override def apply(s: Out): Vector[Out] = {
        for {
          h ← (0 until s.head).toVector
          t ← tail(s.tail)
        } yield
          h :: t
      }
    }
  }
}
