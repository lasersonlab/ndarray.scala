package org.lasersonlab.anndata

import org.lasersonlab.ndarray.Ints._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Group

case class AnnData[Obs, Var](
     X : zarr.Array.S[Ints2, Float],
   obs : zarr.Array.S[Ints1,   Obs],
  `var`: zarr.Array.S[Ints1,   Var],
   uns : Group
)
