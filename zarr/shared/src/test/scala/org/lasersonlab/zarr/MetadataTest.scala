package org.lasersonlab.zarr

import _root_.io.circe.parser._
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.Ints._
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
          fill_value = ""
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
           dtype = float,
          fill_value = 0.0f
        )
      )
    )
  }
}
