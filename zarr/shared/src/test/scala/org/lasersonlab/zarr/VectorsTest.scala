package org.lasersonlab.zarr

import cats.implicits._

class VectorsTest
  extends hammerlab.Suite {
  test("1-D") {
    val arr =
      Vctrs(
        10 to 20 toVector,
        30 to 40 toVector,
        50 to 60 toVector,
        70 to 80 toVector
      )

    arr.foldLeft(0)(_ + _) should be(1980)

    arr.map(_ * 2).foldLeft(0)(_ + _) should be(3960)
  }

  test("2-D") {
    val arr =
      Vctrs(
        Vector(
          10 to 20 toVector,
          30 to 40 toVector
        ),
        Vector(
          50 to 60 toVector,
          70 to 80 toVector
        )
      )

    arr.foldLeft(0)(_ + _) should be(1980)

    arr.map(_ * 2).foldLeft(0)(_ + _) should be(3960)
  }

  test("3-D") {
    val arr =
      Vctrs(
        Vector(
          Vector(
              0 until  10 toVector,
             10 until  20 toVector,
             20 until  30 toVector
          ),
          Vector(
             30 until  40 toVector,
             40 until  50 toVector,
             50 until  60 toVector
          )
        ),
        Vector(
          Vector(
             60 until  70 toVector,
             70 until  80 toVector,
             80 until  90 toVector
          ),
          Vector(
             90 until 100 toVector,
            100 until 110 toVector,
            110 until 120 toVector
          )
        )
      )

    arr.foldLeft(0)(_ + _) should be(7140)

    arr.map(_ * 2).foldLeft(0)(_ + _) should be(14280)
  }
}
