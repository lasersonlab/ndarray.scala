package org.lasersonlab

import org.hammerlab.paths.HasPathOps
import org.lasersonlab.ndarray.Arithmetic
import org.lasersonlab.zarr.opt.OptDecoder
import org.lasersonlab.zarr.tlist.TListDecoders

package object zarr
  extends Arithmetic.HasOps
     with OptDecoder
     with TListDecoders
     with HasPathOps
     with hammerlab.either
     with hammerlab.math.utils
