package org.lasersonlab.zarr

import hammerlab.path._

class GroupTest
  extends hammerlab.Suite {
  test("dir") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.ad.32m.zarr")
    val group @ Group(arrays, groups) = Group(path).fold(fail(_), identity)
    arrays.size should be(3)
    groups.size should be(1)
  }
}
