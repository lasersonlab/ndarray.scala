package org.lasersonlab

import org.hammerlab.paths.HasPathOps
import org.lasersonlab.ndarray.Arithmetic
import org.lasersonlab.zarr.opt.OptCodec
import org.lasersonlab.zarr.tlist.TListCodec

package object zarr
  extends Arithmetic.HasOps
     with OptCodec
     with TListCodec
     with HasPathOps
     with hammerlab.either
     with hammerlab.math.utils
