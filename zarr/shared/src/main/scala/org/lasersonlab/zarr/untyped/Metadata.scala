package org.lasersonlab.zarr.untyped

import hammerlab.option._
import hammerlab.path._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import org.lasersonlab.zarr.FillValue.Null
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr.Metadata._
import org.lasersonlab.zarr._
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.group.Basename

case class Metadata(
   shape: Seq[Int],
  chunks: Seq[Int],
  dtype: DataType,
  compressor: Compressor,
  order: Order,
  fill_value: FillValue[Json] = Null,
  zarr_format: Format = `2`,
  filters: Opt[Seq[Filter]] = None
) {
  type T = dtype.T
  require(shape.size == chunks.size)
  val rank = shape.size
}
object Metadata {
  type Aux[_T] = Metadata { type T = _T }
  def apply(dir: Path): Exception | Metadata =
    dir ? basename flatMap {
      path ⇒
        decode[Metadata](path.read)
    }

  implicit val _basename = Basename[Metadata](basename)
}
