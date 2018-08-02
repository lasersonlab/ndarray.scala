package org.lasersonlab.ndarray

import Repd.Aux
import shapeless.nat._

class RepdTest
  extends hammerlab.Suite {
  test("derivations") {
    implicitly[Repd[Int]]
    implicitly[Aux[Int, _0, Int]]

    implicitly[Repd[Seq[Int]]]
    implicitly[Aux[Seq[Int], _1, Seq[Int]]]
    implicitly[Aux[Vector[Int], _1, Seq[Int]]]
    implicitly[Aux[Range, _1, Seq[Int]]]
    implicitly[Aux[Range.Inclusive, _1, Seq[Int]]]
    implicitly[Aux[Range, _1, Seq[Int]]]
    implicitly[Repd[Range.Inclusive]]
    implicitly[Repd[Range]]

    implicitly[Repd[Seq[Seq[Int]]]]
    implicitly[Aux[Seq[Seq[Int]], _2, Seq[Seq[Int]]]]
    implicitly[Aux[Vector[IndexedSeq[Int]], _2, Seq[Seq[Int]]]]

    implicitly[Repd[Seq[Seq[Seq[Int]]]]]
    implicitly[Aux[Seq[Seq[Seq[Int]]], _3, Seq[Seq[Seq[Int]]]]]
  }
}
