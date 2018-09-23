package org.lasersonlab.anndata

import org.lasersonlab.anndata.AnnData.Idx
import org.lasersonlab.shapeless.Shape._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Group

case class AnnData[Obs, Var](
     X : zarr.Array.S[_2, Idx, Float],
   obs : zarr.Array.S[_1, Idx,   Obs],
  `var`: zarr.Array.S[_1, Idx,   Var],
   uns : Group[Idx]
)

object AnnData {
  type Idx = Int
}
