package org.lasersonlab.ndarray.fresh

import hammerlab.shapeless._
import shapeless._
import nat._
import org.lasersonlab.ndarray.fresh.Array.Arg
import org.lasersonlab.ndarray.fresh.Array.Idx

class ArrayTest
  extends hammerlab.Suite {
  test("one") {
  }

  test("two") {
    // Constructing from different Seq types is allowed
    Array(10 to 20 toVector, 30 to 40, 50 to 60)
    Array(10 to 20, 30 to 40: Seq[Int], 50 to 60 toVector)

    // This won't compile; the dimensions are wrong!
    // Array(10 to 20, 30 to 40, 50)

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
    } {
      a(r, c) should be(10 + 20 * r + c)
      a((r, c)) should be(10 + 20 * r + c)
    }

    // none of these compile; dimensions are wrong:
    // a(r, r, c)
    // a((r, r, c))
    // a(r)
    // a((r))
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
    } {
      a(x, y, z) should be(10 + 40*x + 20*y + z)
      a((x, y, z)) should be(10 + 40*x + 20*y + z)
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
    } withClue(s"$sgn $x $y $z") {
      a(w, x, y, z) should be(sgn * (10 + 40*x + 20*y + z))
      a((w, x, y, z)) should be(sgn * (10 + 40*x + 20*y + z))
    }

  }

  test("args") {
    val r = (10 to 20): Arg[Seq[Int], _1]
//    val r = (10 to 20): Arg[Range.Inclusive]
    r.out should be(10 to 20 toSeq)

    Seq(10 to 20, 30 to 40): Arg[Seq[Seq[Int]], _2]
    Seq(Seq(10 to 20, 30 to 40), Seq(10 to 20, 30 to 40)): Arg[Seq[Seq[Seq[Int]]], _3]
  }
}
