package org.lasersonlab.zarr

import io.circe.Decoder

sealed trait Filter
object Filter {
  implicit val decoder: Decoder[Filter] = ???
}
