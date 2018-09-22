package org.lasersonlab.zarr

import circe.parser._
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.Ints._
import org.lasersonlab.ndarray.TList.{ `1`, `2` }
import org.lasersonlab.zarr.dtype.ByteOrder.LittleEndian
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.data$._

class MetadataTest
  extends Suite {

  test("1-D chars") {
    import org.lasersonlab.ndarray.TList.traverses.traverse_1
    decode[
      Metadata[
        String,
        `1`,
        Int
      ]
    ](
      `col_attrs/DonorID/.zarray`
    ) should be(
      Right(
        Metadata[
          String,
          `1`,
          Int
        ](
           shape = Dimension(5425) :: TNil,
           dtype = string(1),
          fill_value = ""
        )
      )
    )
  }

  test("2-D floats") {
    import org.lasersonlab.ndarray.TList.traverses.traverse_2
    decode[
      Metadata[
        Float,
        `2`,
        Int
      ]
    ](
      `matrix/.zarray`
    ) should be(
      Right(
        Metadata[
          Float,
          `2`,
          Int
        ](
           shape = Dimension(27998) :: Dimension(5425) :: TNil,
           dtype = float,
          fill_value = 0.0f
        )
      )
    )
  }
}
