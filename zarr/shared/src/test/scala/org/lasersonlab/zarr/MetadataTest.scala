package org.lasersonlab.zarr

import circe.Encoder
import circe.parser._
import org.lasersonlab.zarr.utils.slist.{ Codec, HKTCodec }
//import hammerlab.shapeless.tlist._
import lasersonlab.shapeless.slist._
//import org.lasersonlab.shapeless.Shape.{ `1`, `2` }
import org.lasersonlab.zarr.dtype.ByteOrder.LittleEndian
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.data$._

class MetadataTest
  extends Suite {

  test("1-D chars") {
    // TODO: make providing traverse et al smoother
    //import org.lasersonlab.shapeless.Shape.instances.traverse_1
    !![HKTCodec[`1`]]
    !![Encoder[Int]]
    !![Codec[`1`[Int]]]
    !![Encoder[`1`[Int]]]
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
           shape = Dimension(5425) :: ⊥,
           dtype = string(1),
          fill_value = ""
        )
      )
    )
  }

  test("2-D floats") {
    //import org.lasersonlab.shapeless.Shape.instances.traverse_2
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
           shape = Dimension(27998) :: Dimension(5425) :: ⊥,
           dtype = float,
          fill_value = 0.0f
        )
      )
    )
  }
}
