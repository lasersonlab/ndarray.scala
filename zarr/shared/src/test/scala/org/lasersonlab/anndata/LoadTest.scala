package org.lasersonlab.anndata

import hammerlab.path._
import org.lasersonlab.anndata.loom.Var
import org.lasersonlab.zarr.Suite
import org.lasersonlab.zarr.group.Load
import org.lasersonlab.zarr.untyped.Struct
import shapeless.{ Path ⇒ _ }

class LoadTest
  extends Suite {
  test("load") {
    import Load.Ops

    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.ad.32m.zarr")
    val ad = path.load[AnnData[Struct, Var]].get
    println(ad)
  }
}
