package org.lasersonlab.zarr.cmp

import cats.{ Eq, data }
import cats.data.{ Ior, NonEmptyList }
import hammerlab.either._
import magnolia._
import org.hammerlab.{ test ⇒ t }
import org.scalatest.FunSuite

import scala.language.experimental.macros

/**
 * Wrapper for [[t.Cmp]] that uses [[magnolia]] derivation
 *
 * - works around https://github.com/milessabin/shapeless/issues/868
 * - loses type-level [[Diff]]-type propagation: Magnolia derivation doesn't allow for supporting them
 */
trait Cmp[T] {
  type Diff
  def apply(l: T, r: T): Option[Diff]
}

object Cmp {
  type Aux[T, D] = Cmp[T] { type Diff = D }
  type Typeclass[T] = Cmp[T]

  def combine[T](ctx: CaseClass[Cmp, T]): Aux[T, NonEmptyList[(String, Any)]] =
    Cmp {
      (l, r) ⇒
        NonEmptyList.fromList(
          ctx
            .parameters
            .toList
            .flatMap {
              p ⇒
                p.typeclass(
                  p.dereference(l),
                  p.dereference(r)
                )
                .map {
                  d ⇒
                    p.label → (d: Any)
                }
                .toList
            }
        )
    }

  def dispatch[T](ctx: SealedTrait[Cmp, T]): Cmp[T] =
    Cmp {
      (l, r) ⇒
        ctx
          .subtypes
          .flatMap {
            t ⇒
              val fn = t.cast.lift
              (
                fn(l),
                fn(r)
              ) match {
                case (Some(l), Some(r)) ⇒
                  t.typeclass(l, r).map(_.toString)
                case (None, None) ⇒
                  None
                case _ ⇒
                  Some(s"Different types: $l $r")
              }
          }
          .headOption
    }

  implicit def gen[T]: Cmp[T] = macro Magnolia.gen[T]

  def apply[T, D](fn: (T, T) ⇒ Option[D]): Aux[T, D] = new Cmp[T] { type Diff = D; def apply(l: T, r: T): Option[D] = fn(l, r) }

  implicit def fromEq[T](implicit e: Eq[T]): Cmp[T] =
    Cmp {
      (l, r) ⇒
        if (e.eqv(l, r))
          None
        else
          Some((l, r))
    }

  implicit def fromSeq[T](implicit e: Cmp[T]): Cmp[Seq[T]] =
    Cmp {
      (l, r) ⇒
        Ior.fromOptions(
          NonEmptyList.fromList(
            l
              .zip(r)
              .zipWithIndex
              .toList
              .flatMap {
                case ((l, r), i) ⇒
                  e(l, r).map { i → _ }.toList
              }
          ),
          if (l.size > r.size)
            Some(Left(l.drop(r.size)))
          else if (r.size > l.size)
            Some(Left(r.drop(l.size)))
          else
            None
        )
    }

  implicit def fromMap[K, V](implicit v: Cmp[V]): Cmp[Map[K, V]] =
    Cmp {
      (l, r) ⇒
        val lk = l.keySet
        val rk = r.keySet
        val  diffs = NonEmptyList.fromList((for { (k, lv) ← l; rv ← r.get(k); d ← v(lv, rv) } yield k → d ) toList)
        val  lefts = NonEmptyList.fromList((for { (k, v) ← l; if !r.contains(k) } yield k → v) toList)
        val rights = NonEmptyList.fromList((for { (k, v) ← r; if !l.contains(k) } yield k → v) toList)

        Ior.fromOptions(
          Ior.fromOptions(lefts, rights),
          diffs
        )
    }

  trait syntax {
    self: FunSuite ⇒
    def eqv[T](l: T, r: T)(implicit ceq: Cmp[T]) =
      ceq(l, r)
        .foreach {
          d ⇒
            fail(d.toString)
        }
  }
}
