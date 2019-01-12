package org.lasersonlab.test

package object future {
  type Cmp[T] = CanEq[T, T]
  object Cmp {
    type Aux[T, D] = CanEq[T, T] { type Diff = D }
  }
}
