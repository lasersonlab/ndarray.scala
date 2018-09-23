package org.lasersonlab.zarr

import lasersonlab.shapeless.slist._
import org.lasersonlab.shapeless.Shape

class KeyTest
  extends hammerlab.Suite
    with Shape.instances {
  test("strings") {
    ==(Key(123               :: ⊥), "123")
    ==(Key(123 :: 456        :: ⊥), "123.456")
    ==(Key(123 :: 456 :: 789 :: ⊥), "123.456.789")
  }
}
