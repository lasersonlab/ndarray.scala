package org.lasersonlab.zarr

import org.scalatest.FunSuite

trait HasGetOps {
  self: FunSuite ⇒
  implicit class GetOps[T](val t: Either[Exception, T]) {
    def get: T = t.fold(fail(_), identity)
  }
}

abstract class Suite
  extends hammerlab.Suite
     with HasGetOps
