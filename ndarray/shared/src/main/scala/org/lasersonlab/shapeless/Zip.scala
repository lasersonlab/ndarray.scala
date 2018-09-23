package org.lasersonlab.shapeless

trait Zip[F[_]] {
  def apply[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}
object Zip {
  implicit class Ops[F[_], A](val fa: F[A]) extends AnyVal {
    def zip[B](fb: F[B])(implicit zip: Zip[F]) = zip(fa, fb)
  }
  trait syntax {
    @inline def zipOps[F[_], A](fa: F[A]): Ops[F, A] = Ops(fa)
  }

  implicit val list: Zip[List] =
    new Zip[List] {
      override def apply[A, B](fa: List[A], fb: List[B]): List[(A, B)] = {
        val ai = fa.iterator
        val bi = fb.iterator
        val zipped =
          ai
            .zip(bi)
            .toList

        if (ai.hasNext)
          throw new IllegalArgumentException(
            s"${ai.size} extra elements on left:\n\t${fa.mkString(",")}\n${fb.mkString(",")}"
          )
        if (bi.hasNext)
          throw new IllegalArgumentException(
            s"${bi.size} extra elements on right:\n\t${fa.mkString(",")}\n${fb.mkString(",")}"
          )

        zipped
      }
    }
}
