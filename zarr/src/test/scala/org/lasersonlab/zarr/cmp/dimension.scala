package org.lasersonlab.zarr.cmp

import org.lasersonlab.zarr.Dimension
import Cmp.by

object dimension {
  trait cmp {
    object dimensions {
      // import this to allow metadata to have different chunk-size fields when comparing arrays
      implicit def ignoreChunks[Idx](implicit idx: Cmp[Idx]) =
        by[Dimension[Idx], Idx](_.size)
    }
  }
}
