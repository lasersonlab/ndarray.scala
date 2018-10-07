package org.lasersonlab.xscala

import scala.{ util ⇒ u }

trait shims {
  @inline implicit def makeTryShimOps[T](t: u.Try[T]): Try.Ops[T] = Try.Ops(t)
}
