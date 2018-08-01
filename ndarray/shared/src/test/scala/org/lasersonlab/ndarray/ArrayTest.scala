package org.lasersonlab.ndarray

class ArrayTest
  extends hammerlab.Suite {
  test("1-D") {
    val a = Array(1, 2, 3, 4)
    ==(a(0), 1)
    ==(a(1), 2)
    ==(a(2), 3)
    ==(a(3), 4)
    ==(a.length, 4)
  }

  test("2-D") {

  }
}
