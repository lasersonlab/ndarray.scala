package org.lasersonlab.zarr

import circe._
import org.lasersonlab.zarr.io.Basename

case class Attrs(json: Json)

object Attrs {
  val basename = ".zattrs"
  implicit val _basename = Basename[Attrs](basename)

  implicit val encoder: Encoder[Attrs] = Encoder.instance(_.json)
  implicit val decoder: Decoder[Attrs] = Decoder.instance(c ⇒ Right(Attrs(c.value)))
}
