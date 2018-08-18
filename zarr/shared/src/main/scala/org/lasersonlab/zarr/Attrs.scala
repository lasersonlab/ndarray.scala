package org.lasersonlab.zarr

import hammerlab.option._
import hammerlab.path._
import io.circe.parser._

case class Attrs(json: io.circe.Json)

object Attrs {
  val basename = ".zattrs"
  def apply(dir: Path): Exception | Opt[Attrs] = {
    val path = dir / basename
    if (!path.exists)
      Right(None)
    else
      parse(path.read)
        .map(
          Attrs(_)
        )
  }
}
