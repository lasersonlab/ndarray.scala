package org.lasersonlab.zarr

import org.hammerlab.paths.HasPathOps
import org.lasersonlab.ndarray.Arithmetic
import org.lasersonlab.zarr.io._
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.zarr.utils.opt.OptDecoder

package object untyped
  extends HasPathOps
     with Arithmetic.HasOps
     with OptDecoder
     with Idx.syntax
     with Load.syntax
     with Save.syntax
     with hammerlab.either
