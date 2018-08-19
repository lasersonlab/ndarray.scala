package org.lasersonlab.zarr

import cats.implicits._
import io.circe.generic.auto._
import hammerlab.path._
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.Ints._
import org.lasersonlab.zarr.dtype.ByteOrder.LittleEndian
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Compressor.Blosc.CName
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr.dtype.DataType
import shapeless.the
import shapeless.nat._

class ArrayTest
  extends Suite {

  test("2-D floats") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/matrix")

    val arr = Array[Float, _2](path).get

    val metadata = arr.metadata

    metadata should be(
      Metadata(
        shape = 27998 :: 5425 :: TNil,
        chunks = 3092 :: 5425 :: TNil,
        dtype = float(LittleEndian),
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
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 0 :: 0 :: TNil,     0 :: 0:: TNil,  3092 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 1 :: 0 :: TNil,  3092 :: 0:: TNil,  6184 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 2 :: 0 :: TNil,  6184 :: 0:: TNil,  9276 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 3 :: 0 :: TNil,  9276 :: 0:: TNil, 12368 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 4 :: 0 :: TNil, 12368 :: 0:: TNil, 15460 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 5 :: 0 :: TNil, 15460 :: 0:: TNil, 18552 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 6 :: 0 :: TNil, 18552 :: 0:: TNil, 21644 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 7 :: 0 :: TNil, 21644 :: 0:: TNil, 24736 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 8 :: 0 :: TNil, 24736 :: 0:: TNil, 27828 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked,  170 :: 5425 :: TNil, 9 :: 0 :: TNil, 27828 :: 0:: TNil, 27998 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil)
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

    val arr = Array[Long, _1](path).get

    arr.metadata should be(
      Metadata(
         shape = 27998 :: TNil,
        chunks = 27998 :: TNil,
        dtype = i64(LittleEndian),
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

    val arr = Array[String, _1](path).get

    arr.metadata should be(
      Metadata(
         shape = 5425 :: TNil,
        chunks = 5425 :: TNil,
        dtype = string(5),
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

  test("1-D structs") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.ad.32m.zarr/var")

    import shapeless._

    val arr = Array[Var, _1](path).get

    arr.metadata should be(
      Metadata(
         shape = 27998 :: TNil,
        chunks = 27998 :: TNil,
        dtype = Var.dtype,
        compressor =
          Blosc(
            cname = CName.lz4,
            clevel = 5,
            shuffle = 1,
            blocksize = 0
          ),
        order = C,
        fill_value = Var.empty,
        zarr_format = `2`,
        filters = None
      )
    )

    ==(arr.attrs, None)

    arr.chunks.size should be(1)
    val chunk = arr.chunks(0)
    val bytes = chunk.bytes
    bytes.length should be(1903864)

    chunk.size should be(27998)
    val elems = chunk.toList
    elems.size should be(27998)
    ==(
      elems.take(10),
      Seq(
        Var(0, "ENSMUSG00000022528", gene = 14509, logCV = 0.4815124, logMean =   0.2018570, selected = 1, total = 1616.0, valid = 1),
        Var(1, "ENSMUSG00000058427", gene =  5558, logCV = 3.3051310, logMean = - 5.9969227, selected = 1, total =   22.0, valid = 1),
        Var(2, "ENSMUSG00000015312", gene =  7952, logCV = 0.6754399, logMean = - 0.1837246, selected = 1, total = 1237.0, valid = 1),
        Var(3, "ENSMUSG00000024401", gene = 25608, logCV = 0.0      , logMean =   0.0      , selected = 1, total =    0.0, valid = 1),
        Var(4, "ENSMUSG00000015396", gene =  4382, logCV = 2.4097327, logMean = - 4.5983734, selected = 1, total =   58.0, valid = 1),
        Var(5, "ENSMUSG00000000982", gene =  4230, logCV = 4.8032539, logMean = - 8.8713918, selected = 1, total =    3.0, valid = 1),
        Var(6, "ENSMUSG00000018930", gene =  4231, logCV = 5.2276633, logMean = -10.4563544, selected = 1, total =    1.0, valid = 1),
        Var(7, "ENSMUSG00000038418", gene =  6673, logCV = 0.2999214, logMean =   2.0053807, selected = 1, total = 5641.0, valid = 1),
        Var(8, "ENSMUSG00000042622", gene = 16562, logCV = 1.1246109, logMean = - 1.2789349, selected = 1, total =  579.0, valid = 1),
        Var(9, "ENSMUSG00000053560", gene = 14957, logCV = 0.4013466, logMean =   2.0053807, selected = 1, total = 5641.0, valid = 1)
      )
    )
  }
}

case class Var(
  index: Long,
  accession: String,
  gene: Short,
  logCV: Double,
  logMean: Double,
  selected: Long,
  total: Double,
  valid: Long
)
object Var {
  val empty = Empty[Var]()

  // used for deriving DataType.Aux[Var] below
  private implicit val stringDataType = string(18)
  val dtype = the[DataType.Aux[Var]]
}
