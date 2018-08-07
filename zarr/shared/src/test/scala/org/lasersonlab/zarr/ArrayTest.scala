package org.lasersonlab.zarr

import hammerlab.option
import hammerlab.option.Opt
import hammerlab.path._
import hammerlab.shapeless.tlist._
import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.lasersonlab.zarr.ByteOrder.LittleEndian
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Compressor.Blosc.CName.lz4
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Order.C

class ArrayTest
  extends hammerlab.Suite {

  val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/matrix/.zarray")

  test("typed metadata read") {

    !![Decoder[TNil]]
    !![Decoder[TNil]](tnil)
    //!![Decoder[TNil]](tlistDecoder)

    !![Decoder[Int :: TNil]]
    !![Decoder[Int :: Int :: TNil]]

    !![Decoder[Float]]
    !![Decoder[DataType.Aux[Float]]]

    decode[TNil](""" [] """) should be(Right(TNil))
//    decode[TNil](""" [] """)(tlistDecoder) should be(Right(TNil))
//    decode[TNil](""" [] """)(tlistDecoder[Int, TNil]) should be(Right(TNil))

    //decode[Int :: TNil](""" [1] """)(tlistDecoder[Int, Int :: TNil]) should be(Right(1 :: TNil))
    decode[Int :: TNil](""" [1] """)(tlistDecoder) should be(Right(1 :: TNil))
    decode[Int :: TNil](""" [1] """) should be(Right(1 :: TNil))

    decode[Int :: Int :: TNil](""" [1, 2] """) should be(Right(1 :: 2 :: TNil))

    val metadata = decode[Metadata[Float, Int :: Int :: TNil]](path.read)

    metadata should be(
      Right(
        Metadata[Float, Int :: Int :: TNil](
          shape = 27998 :: 5425 :: TNil,
          chunks = 3092 :: 5425 :: TNil,
          dtype = DataType.float(LittleEndian),
          compressor =
            Blosc(
              cname = lz4,
              clevel = 5,
              shuffle = 1,
              blocksize = 0
            ),
          order = C,
          fill_value = 0.0f,
          zarr_format = `2`,
          filters = None
        )
      )
    )
  }

  test("untyped metadata read") {
    val metadata = decode[Metadata.untyped.Metadata](path.read)
    metadata should be(
      Right(
        Metadata.untyped.Metadata(
          shape = Seq(27998, 5425),
          chunks = Seq(3092, 5425),
          dtype = DataType.float(LittleEndian),
          compressor =
            Blosc(
              cname = lz4,
              clevel = 5,
              shuffle = 1,
              blocksize = 0
            ),
          order = C,
          fill_value = 0.0,
          zarr_format = `2`,
          filters = None
        )
      )
    )
  }
}
