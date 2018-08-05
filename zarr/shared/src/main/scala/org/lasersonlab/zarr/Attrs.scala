package org.lasersonlab.zarr

import java.io.FileNotFoundException

import hammerlab.path._
import io.circe.parser._

case class Attrs(json: io.circe.Json)

object Attrs {
  val basename = ".zattrs"

  def apply(dir: Path): Either[Exception, Attrs] = {
    val path = dir / basename
    if (!path.exists)
      Left(
        new FileNotFoundException(
          path.toString
        )
      )
    else
      parse(path.read)
        .map(
          Attrs(_)
        )
  }
}
