package org.lasersonlab.zarr

import hammerlab.shapeless.tlist._
import io.circe.Json._
import io.circe.generic.auto._
import io.circe.parser._
import org.lasersonlab.ndarray.Ints._
import org.lasersonlab.zarr.ByteOrder.LittleEndian
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Compressor.Blosc.CName.lz4
import org.lasersonlab.zarr.DataType._
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Order.C

class MetadataTest
  extends hammerlab.Suite {

  val `matrix/.zarray` =
    """{
      |    "chunks": [
      |        3092,
      |        5425
      |    ],
      |    "compressor": {
      |        "blocksize": 0,
      |        "clevel": 5,
      |        "cname": "lz4",
      |        "id": "blosc",
      |        "shuffle": 1
      |    },
      |    "dtype": "<f4",
      |    "fill_value": 0.0,
      |    "filters": null,
      |    "order": "C",
      |    "shape": [
      |        27998,
      |        5425
      |    ],
      |    "zarr_format": 2
      |}"""
      .stripMargin

  val `col_attrs/DonorID/.zarray` =
    """{
      |    "chunks": [
      |        5425
      |    ],
      |    "compressor": {
      |        "blocksize": 0,
      |        "clevel": 5,
      |        "cname": "lz4",
      |        "id": "blosc",
      |        "shuffle": 1
      |    },
      |    "dtype": "|S1",
      |    "fill_value": "",
      |    "filters": null,
      |    "order": "C",
      |    "shape": [
      |        5425
      |    ],
      |    "zarr_format": 2
      |}"""
      .stripMargin

  test("typed metadata read: 1-D chars") {
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
          compressor =
            Blosc(
              cname = lz4,
              clevel = 5,
              shuffle = 1,
              blocksize = 0
            ),
          order = C,
          fill_value = "",
          zarr_format = `2`
        )
      )
    )
  }

  test("untyped metadata read: 1-D chars") {
    decode[
      Metadata.untyped.Metadata
    ](
      `col_attrs/DonorID/.zarray`
    ) should be(
      Right(
        Metadata.untyped.Metadata(
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

  test("typed metadata read: 2-D floats") {
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
          compressor =
            Blosc(
              cname = lz4,
              clevel = 5,
              shuffle = 1,
              blocksize = 0
            ),
          order = C,
          fill_value = 0.0f,
          zarr_format = `2`
        )
      )
    )
  }

  test("untyped metadata read") {
    decode[
      Metadata.untyped.Metadata
    ](
      `matrix/.zarray`
    ) should be(
      Right(
        Metadata.untyped.Metadata(
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
