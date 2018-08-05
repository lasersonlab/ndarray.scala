package org.lasersonlab.zarr

import java.io.FileNotFoundException

import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser._
import org.lasersonlab.zarr.Format._

case class Metadata[T, Shape](
  shape: Shape,
  chunks: Shape,
  dtype: DataType[T],
  compressor: Compressor,
  order: Order,
  fill_value: Opt[T] = None,
  zarr_format: Format = `2`,
  filters: Seq[Filter] = Nil
)

object Metadata {
  val basename = ".zarray"
  def apply[
    T : Decoder,
    Shape : Decoder
  ](
    dir: Path
  ):
    Either[
      Exception,
      Metadata[T, Shape]
    ] = {
    val path = dir / basename
    implicitly[Decoder[DataType[T]]]
    implicitly[Decoder[Compressor]]
    implicitly[Decoder[Order]]
    implicitly[Decoder[Opt[T]]]
    implicitly[Decoder[Seq[Filter]]]
    implicitly[Decoder[Metadata[T, Shape]]]
    if (!path.exists)
      Left(
        new FileNotFoundException(
          path.toString
        )
      )
    else
      decode[Metadata[T, Shape]](path.read)
  }
}
