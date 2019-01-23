package org.lasersonlab

import cats.MonadError
import hammerlab.option.Opt

package object uri
extends uri.syntax
   with lasersonlab.future
   with lasersonlab.opt.circe {
  type MonadErr[F[_]] = MonadError[F, Throwable]
  type ?[+T] = Opt[T]
  type Mod[T] = T ⇒ T
  type   Δ[T] = T ⇒ T

  implicit class ΔOps[T](val Δ: Δ[T]) extends AnyVal {
    def as[U](implicit p: Patch[U, T]): Δ[U] = (u: U) ⇒ p(u, Δ)
  }
  implicit def widenΔ[T, U](Δ: Δ[T])(implicit p: Patch[U, T]): Δ[U] = p(_, Δ)
}
