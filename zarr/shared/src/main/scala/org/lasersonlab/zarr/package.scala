package org.lasersonlab

import org.lasersonlab.ndarray.Arithmetic

package object zarr
  extends TListDecoders
     with OptDecoder
     with Arithmetic.HasOps {

  // TODO: move these to appropriate dependencies

  type |[+L, +R] = Either[L, R]

  object Int {
    def unapply(s: List[Char]): Option[Int] = Some( s.mkString.toInt )
  }
}
