package org.lasersonlab.ndarray.fresh

import hammerlab.shapeless._
import shapeless._
import nat._
import shapeless.test.illTyped

class ArrayTest
  extends hammerlab.Suite {

  implicit val __0 = _0

  test("one") {
    val a = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    a.n.toInt should be(1)

    for {
      i ← 0 until 10
    } {
      a(i) should be(i + 1)
    }

    illTyped("a(0, 0)")
  }

  test("two") {
    // Constructing from different Seq types is allowed
    Array(10 to 20 toVector, 30 to 40, 50 to 60)
    Array(10 to 20, 30 to 40: Seq[Int], 50 to 60 toVector)

    // This won't compile; the dimensions are wrong!
    illTyped("Array(10 to 20, 30 to 40, 50)")

    Array(
      10 to 20,
      30 to 40,
      50 to 60
    )

    val a: Array[Int, _2] =
      Array(
        10 to 20,
        30 to 40,
        50 to 60
      )

    for {
      r ← 0 to 2
      c ← 0 to 10
      expected = 10 + 20 * r + c
    } {
      a( r, c ) should be(expected)
      a((r, c)) should be(expected)
    }

    // none of these compile; dimensions are wrong:
    illTyped("a(r, r, c)")
    illTyped("a((r, r, c))")
    illTyped("a(r)")
    illTyped("a((r))")
  }

  test("three") {
    // specify the
    val a =
      Array(
        Seq(
          10 to  20: Seq[Int],
          30 to  40: Seq[Int],
          50 to  60: Seq[Int]
        ),
        Seq(
          50 to  60: Seq[Int],
          70 to  80: Seq[Int],
          90 to 100: Seq[Int]
        )
      )

    a.n.toInt should be(3)

    // here's the inferred type, in case you need/want it
    a: Array[Int, _3]

    for {
      x ← 0 to  1
      y ← 0 to  2
      z ← 0 to 10
      expected = 10 + 40*x + 20*y + z
    } {
      a( x, y, z ) should be(expected)
      a((x, y, z)) should be(expected)
    }
  }

  test("four") {
    val a =
      Array(
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
        ),
        Seq(
          Seq(
            -10 to - 20 by -1,
            -30 to - 40 by -1,
            -50 to - 60 by -1
          ),
          Seq(
            -50 to - 60 by -1,
            -70 to - 80 by -1,
            -90 to -100 by -1
          )
        )
      )

    a.n.toInt should be(4)

    for {
      w ← 0 to 1
      sgn = 1 - 2*w
      x ← 0 to  1
      y ← 0 to  2
      z ← 0 to 10
      expected = sgn * (10 + 40*x + 20*y + z)
    } withClue(s"$sgn $x $y $z") {
      a( w, x, y, z ) should be(expected)
      a((w, x, y, z)) should be(expected)
    }
  }
}
