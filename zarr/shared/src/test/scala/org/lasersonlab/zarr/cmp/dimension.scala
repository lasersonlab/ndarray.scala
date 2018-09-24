package org.lasersonlab.zarr.cmp

import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.Dimension

object dimension {
  trait cmp {
    object dimensions {
      // import this to allow metadata to have different chunk-size fields when comparing arrays
      implicit def ignoreChunks[Idx](implicit idx: Cmp[Idx]) =
        Cmp.by[Idx, Dimension[Idx]](_.arr)
    }
  }
}
