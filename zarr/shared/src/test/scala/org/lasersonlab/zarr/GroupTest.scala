package org.lasersonlab.zarr

import hammerlab.path._

class GroupTest
  extends hammerlab.Suite {
  test("dir") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.ad.32m.zarr")
    val group @ Group(arrays, groups, attrs) = Group(path).fold(fail(_), identity)
    arrays.keys should be(Set("X", "obs", "var"))
    groups.keys should be(Set("uns"))
    ==(attrs, None)
  }
}
