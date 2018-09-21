package org.lasersonlab.anndata

import org.lasersonlab.ndarray.TList._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Group

case class AnnData[Obs, Var](
     X : zarr.Array.S[_2, Int, Float],
   obs : zarr.Array.S[_1, Int,   Obs],
  `var`: zarr.Array.S[_1, Int,   Var],
   uns : Group
)
