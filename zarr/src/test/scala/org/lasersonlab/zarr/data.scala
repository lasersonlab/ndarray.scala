package org.lasersonlab.zarr

trait data {
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
}

object data extends data
