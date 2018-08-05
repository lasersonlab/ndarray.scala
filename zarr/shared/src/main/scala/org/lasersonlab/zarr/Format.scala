package org.lasersonlab.zarr

import io.circe.Decoder

sealed trait Format
object Format {
  // Zarr version 1 not supported (yet?)
  // case object `1` extends Format

  case object `2` extends Format

  implicit val decoder: Decoder[Format] = ???
}
