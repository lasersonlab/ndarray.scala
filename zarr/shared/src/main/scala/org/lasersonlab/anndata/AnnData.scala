package org.lasersonlab.anndata

import org.lasersonlab.ndarray.Ints._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.untyped.Group

case class AnnData[Obs, Var](
     X : zarr.Array[Float, Ints2],
   obs : zarr.Array[  Obs, Ints1],
  `var`: zarr.Array[  Var, Ints1],
   uns : Group
)
