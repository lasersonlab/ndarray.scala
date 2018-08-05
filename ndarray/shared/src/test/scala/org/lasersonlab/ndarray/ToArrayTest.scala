package org.lasersonlab.ndarray

import hammerlab.shapeless.tlist._

class ToArrayTest
  extends hammerlab.Suite {

  test("empty") {
    val a = Array()
    a.shape should be(TNil)
  }

  test("1-D") {

    val a = !![ToArray[Seq[Int]]]
    val s = 1 to 10

    Array(s)
    s: Array[Int]
    (s: Seq[Int]): Array[Int]

    a.shape(s) should be(10 :: TNil)
    for {
      i ← 0 until 10
    } {
      a(s, i :: TNil) should be(i + 1)
    }
  }

  test("2-D") {
    val a = shapeless.the[ToArray[Seq[Seq[Int]]]]
    val s =
      Seq(
        10 to 20,
        30 to 40
      )

    Array(s)
    s: Array[Int]

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

    val arr: Array.Aux[Int, Int :: Int :: Int :: TNil] = Array(s)

    !![ToList[Int, arr.Idx]]
    !![ToList[Int, Int :: TNil]]
    !![ToList[Int, Int :: Int :: TNil]]
    !![ToList[Int, Int :: Int :: Int :: TNil]]

    s: Array[Int]

    {
      import ToArray.Ops
      s.shape should be(2 :: 3 :: 11 :: TNil)
      a.shape(s) should be(2 :: 3 :: 11 :: TNil)
    }

    for {
      x ← 0 to  1
      y ← 0 to  2
      z ← 0 to 10
    } {
      a(s, x :: y :: z :: TNil) should be(10 + 40*x + 20*y + z)
    }

    import Write.Ops

    val bs = arr.write

    val bytes = Bytes[Int](s.write)(arr.shape)
    bytes.bytes.length should be(2 * 3 * 11 * 4)
    bytes.shape should be(arr.shape)

    for {
      x ← 0 to  1
      y ← 0 to  2
      z ← 0 to 10
    } {
      bytes(x :: y :: z :: TNil) should be(10 + 40*x + 20*y + z)
    }
  }
}
