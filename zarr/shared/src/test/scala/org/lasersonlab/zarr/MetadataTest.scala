package org.lasersonlab.zarr

import hammerlab.shapeless.tlist._
import io.circe.parser._
import org.lasersonlab.ndarray.Ints._
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr.dtype.ByteOrder.LittleEndian
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.utils._

class MetadataTest
  extends Suite {

  test("1-D chars") {
    decode[
      Metadata[
        String,
        Ints1
      ]
    ](
      `col_attrs/DonorID/.zarray`
    ) should be(
      Right(
        Metadata[
          String,
          Ints1
        ](
           shape = 5425 :: TNil,
          chunks = 5425 :: TNil,
          dtype = string(1),
          compressor = Blosc(),
          order = C,
          fill_value = "",
          zarr_format = `2`
        )
      )
    )
  }

  test("2-D floats") {
    decode[
      Metadata[
        Float,
        Ints2
      ]
    ](
      `matrix/.zarray`
    ) should be(
      Right(
        Metadata[
          Float,
          Ints2
        ](
           shape = 27998 :: 5425 :: TNil,
          chunks =  3092 :: 5425 :: TNil,
          dtype = float(LittleEndian),
          compressor = Blosc(),
          order = C,
          fill_value = 0.0f,
          zarr_format = `2`
        )
      )
    )
  }
}
