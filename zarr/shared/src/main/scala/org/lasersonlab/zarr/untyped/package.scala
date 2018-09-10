package org.lasersonlab.zarr

import org.hammerlab.paths.HasPathOps
import org.lasersonlab.zarr.io._
import org.lasersonlab.zarr.opt.OptDecoder

package object untyped
  extends HasPathOps
     with OptDecoder
     with Load.syntax
     with Save.syntax
