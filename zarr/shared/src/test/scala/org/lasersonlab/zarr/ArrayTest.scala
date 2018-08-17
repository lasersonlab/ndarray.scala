package org.lasersonlab.zarr

import cats.implicits._
import hammerlab.path._
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.Bytes
import org.lasersonlab.ndarray.Vectors.{ Vector1, Vector2 }
import org.lasersonlab.zarr.ByteOrder.LittleEndian
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Compressor.Blosc.CName
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Ints.{ Ints1, Ints2 }
import org.lasersonlab.zarr.Order.C
import shapeless.nat._

class ArrayTest
  extends hammerlab.Suite {
  val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/matrix")
  test("2-D floats") {

    implicit val float = DataType.float(LittleEndian)

    val arr =
      Array[Float, _2](path)
        .right
        .get

    val metadata = arr.metadata

    metadata should be(
      Metadata(
        shape = 27998 :: 5425 :: TNil,
        chunks = 3092 :: 5425 :: TNil,
        dtype = float,
        compressor =
          Blosc(
            cname = CName.lz4,
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

    ==(arr.attrs, None)

    val rows = arr.chunks.rows
    arr.chunks.size should be(10)
    rows.length should be(10)
    rows.foreach(_.size should be(1))
    val chunks = rows.map(_.head)

    val unchecked = scala.Array.fill(1 << 26)(0.toByte)

    val expected =
      Seq(
        Chunk(unchecked, 3092 :: 5425 :: TNil, 0 :: 0 :: TNil,     0 :: 0:: TNil,  3092 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk(unchecked, 3092 :: 5425 :: TNil, 1 :: 0 :: TNil,  3092 :: 0:: TNil,  6184 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk(unchecked, 3092 :: 5425 :: TNil, 2 :: 0 :: TNil,  6184 :: 0:: TNil,  9276 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk(unchecked, 3092 :: 5425 :: TNil, 3 :: 0 :: TNil,  9276 :: 0:: TNil, 12368 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk(unchecked, 3092 :: 5425 :: TNil, 4 :: 0 :: TNil, 12368 :: 0:: TNil, 15460 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk(unchecked, 3092 :: 5425 :: TNil, 5 :: 0 :: TNil, 15460 :: 0:: TNil, 18552 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk(unchecked, 3092 :: 5425 :: TNil, 6 :: 0 :: TNil, 18552 :: 0:: TNil, 21644 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk(unchecked, 3092 :: 5425 :: TNil, 7 :: 0 :: TNil, 21644 :: 0:: TNil, 24736 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk(unchecked, 3092 :: 5425 :: TNil, 8 :: 0 :: TNil, 24736 :: 0:: TNil, 27828 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk(unchecked,  170 :: 5425 :: TNil, 9 :: 0 :: TNil, 27828 :: 0:: TNil, 27998 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil)
      )

    chunks
      .zip(expected)
      .foreach {
        case (actual: Chunk[Float, Ints2], expected) ⇒
          ==(actual.start, expected.start)
          ==(actual.end, expected.end)
          ==(actual.idx, expected.idx)
          ==(actual.shape, expected.shape)
      }

    val chunkNonzeroCounts =
      chunks
        .map {
          _.foldLeft(0) {
            (sum, n) ⇒
              if (n > 0)
                sum + 1
              else
                sum
          }
        }

    chunkNonzeroCounts should be(
      Vector(
        287412,
        444234,
        17227,
        58283,
        876917,
        796162,
        582618,
        505650,
        615567,
        36142
      )
    )
  }

  test("1-D longs") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/row_attrs/_Valid")

    implicit val long = DataType.i64(LittleEndian)

    val arr =
      Array[Long, _1](path)
        .right
        .get

    arr.metadata should be(
      Metadata(
         shape = 27998 :: TNil,
        chunks = 27998 :: TNil,
        dtype = long,
        compressor =
          Blosc(
            cname = CName.lz4,
            clevel = 5,
            shuffle = 1,
            blocksize = 0
          ),
        order = C,
        fill_value = 0L,
        zarr_format = `2`,
        filters = None
      )
    )

    ==(arr.attrs, None)

    arr.chunks.size should be(1)
    val chunk = arr.chunks(0)
    val bytes = chunk.bytes
    bytes.length should be(223984)

    chunk.size should be(27998)

    val nonzeros =
      chunk.foldLeft(0) {
        (sum, n) ⇒
          if (n > 0)
            sum + 1
          else
            sum
      }

    nonzeros should be(11717)
  }

  test("1-D strings") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/col_attrs/Sex")

    implicit val string = DataType.string(5)

    val arr = Array[String, _1](path).right.get

    arr.metadata should be(
      Metadata(
         shape = 5425 :: TNil,
        chunks = 5425 :: TNil,
        dtype = string,
        compressor =
          Blosc(
            cname = CName.lz4,
            clevel = 5,
            shuffle = 1,
            blocksize = 0
          ),
        order = C,
        fill_value = "",
        zarr_format = `2`,
        filters = None
      )
    )

    ==(arr.attrs, None)

    arr.chunks.size should be(1)
    val chunk = arr.chunks(0)
    val bytes = chunk.bytes
    bytes.length should be(27125)

    chunk.size should be(5425)
    val elems = chunk.toList
    elems.size should be(5425)
    elems.take(10) should be(
      List(
        "1M",
        "F",
        "1F",
        "F",
        "1M 1F",
        "1M 1F",
        "F",
        "1M 1F",
        "F",
        "M"
      )
    )
  }
}
