package org.lasersonlab.zarr

import java.io.FileNotFoundException

import hammerlab.option._
import hammerlab.path._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.{ Decoder, Json }
import org.lasersonlab.zarr.Format._

case class Metadata[T, Shape](
  shape: Shape,
  chunks: Shape,
  dtype: DataType.Aux[T],
  compressor: Compressor,
  order: Order,
  fill_value: Opt[T] = None,
  zarr_format: Format = `2`,
  filters: Opt[Seq[Filter]] = None
)

object Metadata {
  val basename = ".zarray"

  // Implicit unwrappers for some fields
  implicit def _compressor[T, Shape](implicit md: Metadata[_, _]): Compressor = md.compressor
  implicit def   _datatype[T, Shape](implicit md: Metadata[T, _]): DataType.Aux[T] = md.dtype

  def apply[
        T : Decoder,
    Shape : Decoder
  ](
    dir: Path
  )(
    implicit
    d: Decoder[DataType.Aux[T]]
  ):
    Exception |
    Metadata[T, Shape]
  = {
    val path = dir / basename
    if (!path.exists)
      Left(
        new FileNotFoundException(
          path.toString
        )
      )
    else
      decode[
        Metadata[
          T,
          Shape
        ]
      ](
        path.read
      )
  }

  object untyped {
    case class Metadata(
       shape: Seq[Int],
      chunks: Seq[Int],
      dtype: DataType,
      compressor: Compressor,
      order: Order,
      fill_value: Opt[Json] = None,
      zarr_format: Format = `2`,
      filters: Opt[Seq[Filter]] = None
    )
  }
}
