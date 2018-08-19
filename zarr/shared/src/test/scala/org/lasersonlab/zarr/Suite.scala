package org.lasersonlab.zarr

abstract class Suite
  extends hammerlab.Suite {
  implicit class GetOps[T](val t: Either[Exception, T]) {
    def get: T = t.fold(fail(_), identity)
  }
}
