package org.lasersonlab.ndarray

class ToArrayTest
  extends hammerlab.Suite {
  val !! = shapeless.the
  test("1-D") {

    ToArray.seq[Int, Int, TNil]
    !![ToArray.Aux[Seq[Int], Int, Int :: TNil]]

    val a = !![ToArray[Seq[Int]]]
    val s = 1 to 10

    a.shape(s) should be(10 :: TNil)
    for {
      i ← 0 until 10
    } {
      a(s, i :: TNil) should be(i + 1)
    }
  }

  test("2-D") {
    ToArray.seq[Seq[Int], Int, Int :: TNil]

    val idx3: Int :: Int :: TNil = 1 :: 2 :: TNil

    val a = shapeless.the[ToArray[Seq[Seq[Int]]]]
    val s =
      Seq(
        10 to 20,
        30 to 40
      )

    a.shape(s) should be(2 :: 11 :: TNil)

    for {
      r ← 0 to 1
      c ← 0 to 10
    } {
      a(s, r :: c :: TNil) should be(10 + 20*r + c)
    }
  }

  test("3-D") {
    val a = !![ToArray[Seq[Seq[Seq[Int]]]]]
    val s =
      Seq(
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

    Array.fromToArray[Seq[Seq[Seq[Int]]], Int, Int :: Int :: Int :: TNil](s)
    Array.fromToArray(s)
    s: Array[Int]

    a.shape(s) should be(2 :: 3 :: 11 :: TNil)

    for {
      x ← 0 to  1
      y ← 0 to  2
      z ← 0 to 10
    } {
      a(s, x :: y :: z :: TNil) should be(10 + 40*x + 20*y + z)
    }
  }
}
