package org.lasersonlab.zarr

import org.lasersonlab.zarr.group.Basename

case class Attrs(json: io.circe.Json)

object Attrs {
  val basename = ".zattrs"
  implicit val _basename = Basename[Attrs](basename)
}
