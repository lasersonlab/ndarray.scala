package org.lasersonlab.zarr.untyped

import io.circe.Json._
import io.circe.parser.decode
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Compressor.Blosc.CName.lz4
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr.dtype.ByteOrder.LittleEndian
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.utils._
import org.lasersonlab.zarr.{ Suite, untyped }
import io.circe.generic.auto._

class MetadataTest
  extends Suite {
  test("1-D chars") {
    decode[
      untyped.Metadata
      ](
      `col_attrs/DonorID/.zarray`
    ) should be(
      Right(
        untyped.Metadata(
           shape = Seq(5425),
          chunks = Seq(5425),
          dtype = string(1),
          compressor =
            Blosc(
              cname = lz4,
              clevel = 5,
              shuffle = 1,
              blocksize = 0
            ),
          order = C,
          fill_value = fromString(""),
          zarr_format = `2`
        )
      )
    )
  }

  test("2-D floats") {
    decode[
      untyped.Metadata
      ](
      `matrix/.zarray`
    ) should be(
      Right(
        untyped.Metadata(
           shape = Seq(27998, 5425),
          chunks = Seq(3092, 5425),
          dtype = float(LittleEndian),
          compressor =
            Blosc(
              cname = lz4,
              clevel = 5,
              shuffle = 1,
              blocksize = 0
            ),
          order = C,
          fill_value = fromFloatOrNull(0.0f),
          zarr_format = `2`
        )
      )
    )
  }
}
