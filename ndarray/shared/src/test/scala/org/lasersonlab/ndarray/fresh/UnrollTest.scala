package org.lasersonlab.ndarray.fresh

import org.lasersonlab.ndarray.fresh.Unroll.Aux
import shapeless.nat._

class UnrollTest
  extends hammerlab.Suite {
  test("derivations") {
    implicitly[Unroll[Seq[Int]]]
    implicitly[Unroll[Seq[Seq[Int]]]]
    implicitly[Unroll[Seq[Seq[Seq[Int]]]]]

    implicitly[Aux[Int, _0, Int]]
    implicitly[Aux[Seq[Int], _1, Int]]
    implicitly[Aux[Seq[Seq[Int]], _2, Int]]
    implicitly[Aux[Seq[Seq[Seq[Int]]], _3, Int]]
  }
}
