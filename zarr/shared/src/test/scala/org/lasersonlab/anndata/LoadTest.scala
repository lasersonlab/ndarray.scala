package org.lasersonlab.anndata

import org.lasersonlab.zarr.group.Load

class LoadTest
  extends hammerlab.Suite {
  test("load") {
    !![Load[AnnData]]
  }
}
