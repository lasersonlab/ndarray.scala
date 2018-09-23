package org.lasersonlab.zarr.cmp.untyped

import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.Dimension

object metadata {
  trait cmp {
    object metadata {
      // import this to allow metadata to have different chunk-size fields when comparing arrays
      implicit def ignoreChunks[Idx](implicit idx: Cmp[Idx]) = //IgnoreChunks.Yes
        Cmp.by[Idx, Dimension[Idx]](_.arr)
    }
  }
}
