package org.lasersonlab.ndarray

import NSeq.succ
import hammerlab.shapeless.TList, TList.Base
import shapeless._, nat._

class NSeqTest
  extends hammerlab.Suite {
  test("derivations") {
    val the = shapeless.the
    val one = the[NSeq.Aux[Int, _1, Seq[Int]]]
    the[NSeq[Int, _1]]
    the[NSeq.Aux[Int, _1, Seq[Int]]]

    one(TList.Base(1), Seq(1, 2, 3))
    val x: one.Out = Seq(1, 2, 3)

    val two = succ[Int, _1]
    two(TList.Cons(0, Base(1)), Seq(Seq(1, 2, 3)))
    val y: two.Out = Seq(Seq(1, 2, 3))

    val two2 = the[NSeq.Aux[Int, _2, Seq[Seq[Int]]]]
    two2(TList.Cons(0, Base(1)), Seq(Seq(1, 2, 3)))
    val y2: two2.Out = Seq(Seq(1, 2, 3))

    val two3 = the[NSeq[Int, _2]]
    two3(TList.Cons(0, Base(1)), Seq(Seq(1, 2, 3)))
    val y3: two3.Out = Seq(Seq(1, 2, 3))

    succ[Int, _2]

    val three = the[NSeq[Int, _3]]
    val z: three.Out = Seq(Seq(Seq(1, 2, 3)))

    val three3 = the[NSeq.Aux[Int, _3, Seq[Seq[Seq[Int]]]]]
    val z3: three3.Out = Seq(Seq(Seq(1, 2, 3)))
  }
}
