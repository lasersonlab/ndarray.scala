package org.lasersonlab.ndarray

/**
 * Given a [[Shape]] of [[Int]]s, "unfold" into all [[Shape]]s of [[Int]]s whose members are all less than their
 * counterparts in the input [[Shape]]
 *
 * Example:
 *
 * -  in: (2, 3)
 * - out: (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)
 *
 * This would correspond to a [[Shape]] of the form:
 *
 * {{{
 * type Shape[T] = (T, T)
 * }}}
 *
 * Default instances are provided for [[List]] as well as [[org.lasersonlab.shapeless.SList]]s
 */
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

  implicit val unfold0: UnfoldRange[`0`] = new UnfoldRange[`0`] {
    def apply(s: `0`[Int]): Vector[`0`[Int]] = Vector(⊥)
  }
  implicit def cons[Tail[_]](
    implicit
    cons:        Cons[Tail],
    tail: UnfoldRange[Tail]
  ):
    UnfoldRange[cons.Out] = {
    type Shape = cons.Out[Int]
    new UnfoldRange[cons.Out] {
      override def apply(s: Shape): Vector[Shape] = {
        for {
          h ← (0 until s.head).toVector
          t ← tail(s.tail)
        } yield
          h :: t
      }
    }
  }

  implicit val unfold1 = cons[`0`]
  implicit val unfold2 = cons[`1`]
  implicit val unfold3 = cons[`2`]
  implicit val unfold4 = cons[`3`]
  implicit val unfold5 = cons[`4`]
  implicit val unfold6 = cons[`5`]
  implicit val unfold7 = cons[`6`]
  implicit val unfold8 = cons[`7`]
  implicit val unfold9 = cons[`8`]
}
