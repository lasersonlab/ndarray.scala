package org.lasersonlab.slist

import cats.{ Functor, Traverse }
import cats.implicits._

trait Zip[F[_]] {
  def apply[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def apply[A, B, C](fa: F[A], fb: F[B], fc: F[C])(implicit f: Functor[F]): F[(A, B, C)] =
    apply(
      apply(fa, fb),
      fc
    )
    .map {
      case ((a, b), c) ⇒ (a, b, c)
    }
  def withIndex[A, B](fa: F[A], fb: F[B])(implicit t: Traverse[F]): F[(A, B, Int)] =
    apply(fa, fb)
      .mapWithIndex {
        case ((a, b), c) ⇒ (a, b, c)
      }
}
object Zip {
  implicit class Ops[F[_], A](val fa: F[A]) extends AnyVal {
    def zip[B](fb: F[B])(implicit zip: Zip[F]) = zip(fa, fb)
    def zip[B, C](fb: F[B], fc: F[C])(implicit zip: Zip[F], f: Functor[F]) = zip(fa, fb, fc)
    def zipAndIndex[B](fb: F[B])(implicit zip: Zip[F], t: Traverse[F]) = zip.withIndex(fa, fb)
  }
  trait syntax {
    @inline implicit def slistZipOps[F[_], A](fa: F[A]): Ops[F, A] = Ops(fa)
  }
  object syntax extends syntax

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
