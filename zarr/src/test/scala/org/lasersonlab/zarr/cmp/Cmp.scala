package org.lasersonlab.zarr.cmp

import cats.data.NonEmptyList
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
  type Typeclass[T] = Cmp[T]

  def combine[T](ctx: CaseClass[Cmp, T]): Cmp[T] =
    new Cmp[T] {
      type Diff = NonEmptyList[(String, Any)]
      def apply(l: T, r: T): Option[Diff] =
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
    new Cmp[T] {
      type Diff = String
      def apply(l: T, r: T): Option[Diff] =
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

  trait syntax {
    self: FunSuite ⇒
    def eqv[T](l: T, r: T)(implicit ceq: Cmp[T]) = {
      ceq(l, r)
      .foreach {
        d ⇒
          fail(d.toString)
      }
    }
  }
}
