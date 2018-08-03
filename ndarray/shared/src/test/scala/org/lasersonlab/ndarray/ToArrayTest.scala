package org.lasersonlab.ndarray

import shapeless._

class ToArrayTest
  extends hammerlab.Suite {
  val !! = shapeless.the
  test("1-D") {
    val a = !![ToArray[Seq[Int]]]
    val s = 1 to 10
    a.shape(s) should be(10 :: HNil)
    for {
      i ← 0 until 10
    } {
      a(s, i :: HNil) should be(i + 1)
    }
  }

  test("2-D") {
    val a = !![ToArray[Seq[Seq[Int]]]]
    val s =
      Seq(
        10 to 20,
        30 to 40
      )

    a.shape(s) should be(2 :: 11 :: HNil)

    for {
      r ← 0 to 1
      c ← 0 to 10
    } {
      a(s, r :: c :: HNil) should be(10 + 20*r + c)
    }
  }

  test("3-D") {
    val a = !![ToArray[Seq[Seq[Seq[Int]]]]]
    val s =
      Seq(
        Seq(
          10 to  20,
          30 to  40,
          50 to  60
        ),
        Seq(
          50 to  60,
          70 to  80,
          90 to 100
        )
      )

    a.shape(s) should be(2 :: 3 :: 11 :: HNil)

    for {
      x ← 0 to  1
      y ← 0 to  2
      z ← 0 to 10
    } {
      a(s, x :: y :: z :: HNil) should be(10 + 40*x + 20*y + z)
    }
  }
}
