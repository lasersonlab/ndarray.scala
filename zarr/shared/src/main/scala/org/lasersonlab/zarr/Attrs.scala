package org.lasersonlab.zarr

import java.io.FileNotFoundException

import hammerlab.path._
import io.circe.parser._
import hammerlab.option._

case class Attrs(json: io.circe.Json)

object Attrs {
  val basename = ".zattrs"

  def apply(dir: Path): Either[Exception, Opt[Attrs]] = {
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
