package org.lasersonlab.anndata

import lasersonlab.shapeless.slist._
import lasersonlab.zarr._

case class AnnData[Obs, Var](
     X : Array[`2`, Float],
   obs : Array[`1`,   Obs],
  `var`: Array[`1`,   Var],
   uns : Group
)
