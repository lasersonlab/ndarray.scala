package org.lasersonlab.zarr

import lasersonlab.shapeless.slist._

class KeyTest
  extends hammerlab.Suite {
  test("strings") {
    ==(Key(123               :: ⊥), "123")
    ==(Key(123 :: 456        :: ⊥), "123.456")
    ==(Key(123 :: 456 :: 789 :: ⊥), "123.456.789")
  }
}
