package org.lasersonlab.zarr

import Format._

object Group {
  case class Metadata(
    zarr_format: Format = `2`
  )
  object Metadata {
    val basename = ".zgroup"
  }
}
