package org.lasersonlab.zarr

import lasersonlab.shapeless.slist._
import org.lasersonlab.zarr.circe.parser._
import org.lasersonlab.zarr.data._
import org.lasersonlab.zarr.dtype.ByteOrder.LittleEndian
import org.lasersonlab.zarr.dtype.DataType._

class MetadataTest
  extends Suite {

  test("1-D chars") {
    decode[
      Metadata[
        `1`,
        Int,
        String
      ]
    ](
      `col_attrs/DonorID/.zarray`
    ) should be(
      Right(
        Metadata(
           shape = Dimension(5425) :: ⊥,
           dtype = string(1),
          fill_value = ""
        )
      )
    )
  }

  test("2-D floats") {
    decode[
      Metadata[
        `2`,
        Int,
        Float
      ]
    ](
      `matrix/.zarray`
    ) should be(
      Right(
        Metadata(
           shape = Dimension.int(27998, 3092) :: Dimension(5425) :: ⊥,
           dtype = float,
          fill_value = 0.0f
        )
      )
    )
  }
}
