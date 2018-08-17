package org.lasersonlab

import org.lasersonlab.ndarray.Arithmetic

package object zarr
  extends TListDecoders
     with OptDecoder
     with Arithmetic.HasOps {
  type |[+L, +R] = Either[L, R]
}
