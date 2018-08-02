package org.lasersonlab.ndarray.fresh

import shapeless._
import nat._
import org.lasersonlab.ndarray.fresh.Array.Arg
import org.lasersonlab.ndarray.fresh.Array.Idx
import org.lasersonlab.ndarray.fresh.NSeq.Indexed

class ArrayTest
  extends hammerlab.Suite {
  test("one") {
  }

  test("two") {
//    val ns = shapeless.the[NSeq.Aux[_2, λ[T ⇒ Indexed[Indexed[T]]]]]
    //def ns: NSeq.Aux[_2, λ[T ⇒ Indexed[Indexed[T]]]] = ???

    Array[Int, _1, Seq](10 to 20, 30 to 40)
    Array(10 to 20: Seq[Int], 30 to 40: Seq[Int])
    Array(10 to 20: Seq[Int], 30 to 40)
    val a = Array(10 to 20, 30 to 40)
    for {
      r ← 0 to 1
      c ← 0 to 10
    } {
      a(TList(r, c)) should be(10 + 20 * r + c)
    }
  }

  test("three") {
    Array[Int, _2, λ[T ⇒ Seq[Seq[T]]]](Seq(10 to 20: Seq[Int]))
    Array(Seq(10 to 20: Seq[Int]))
    Array(Seq(10 to 20: Seq[Int], 30 to 40))
    Array(Seq(10 to 20, 30 to 40))
    Array(Seq(10 to 20, 30 to 40), Seq(50 to 60, 70 to 80))
    Array(Vector(10 to 20))
  }

  test("args") {
    val r = (10 to 20): Arg[Seq[Int]]
//    val r = (10 to 20): Arg[Range.Inclusive]
    r.out should be(10 to 20 toSeq)
  }
}
