package org.lasersonlab.zarr

import hammerlab.shapeless.tlist._
import org.lasersonlab.shapeless.Shape

class KeyTest
  extends hammerlab.Suite
    with Shape.instances {
  test("strings") {
    ==(Key(123               :: TNil), "123")
    ==(Key(123 :: 456        :: TNil), "123.456")
    ==(Key(123 :: 456 :: 789 :: TNil), "123.456.789")
  }
}
