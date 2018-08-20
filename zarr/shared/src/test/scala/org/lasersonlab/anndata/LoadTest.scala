package org.lasersonlab.anndata

import cats.Foldable
import hammerlab.path._
import org.lasersonlab.anndata.loom.Var
import org.lasersonlab.ndarray.Ints.Ints2
import org.lasersonlab.zarr
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

    import cats.implicits._
    val foldable = !![Foldable[zarr.Array[?, Ints2]]]

    import ad.X._
    foldable.foldLeft[Float, Float](ad.X, 0.0f)(_ + _)
    ad.X.foldLeft(0.0f)(_ + _)
  }
}
