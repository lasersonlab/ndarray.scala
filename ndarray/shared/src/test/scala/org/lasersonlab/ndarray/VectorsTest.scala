package org.lasersonlab.ndarray

import cats.implicits._

class VectorsTest
  extends hammerlab.Suite {
  test("1-D") {
    val arr =
      Vectors(
        10 to 20,
        30 to 40,
        50 to 60,
        70 to 80
      )

    arr.foldLeft(0)(_ + _) should be(1980)

    arr.map(_ * 2).foldLeft(0)(_ + _) should be(3960)
  }

  test("2-D") {
    val arr =
      Vectors(
        Seq(
          10 to 20,
          30 to 40
        ),
        Seq(
          50 to 60,
          70 to 80
        )
      )

    arr.foldLeft(0)(_ + _) should be(1980)

    arr.map(_ * 2).foldLeft(0)(_ + _) should be(3960)
  }

  test("3-D") {
    val arr =
      Vectors(
        Seq(
          Seq(
              0 until  10,
             10 until  20,
             20 until  30
          ),
          Seq(
             30 until  40,
             40 until  50,
             50 until  60
          )
        ),
        Seq(
          Seq(
             60 until  70,
             70 until  80,
             80 until  90
          ),
          Seq(
             90 until 100,
            100 until 110,
            110 until 120
          )
        )
      )

    arr.foldLeft(0)(_ + _) should be(7140)

    arr.map(_ * 2).foldLeft(0)(_ + _) should be(14280)
  }
}
