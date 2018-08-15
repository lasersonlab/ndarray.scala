package org.lasersonlab.zarr

import java.nio.ByteBuffer

import cats.Foldable
import cats.implicits._
import hammerlab.path._
import hammerlab.shapeless.tlist._
import org.blosc.{ JBlosc, PrimitiveSizes, Shuffle }
import org.lasersonlab.ndarray.Vectors.{ Vector1, Vector2 }
import org.lasersonlab.zarr.ByteOrder.LittleEndian
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Compressor.Blosc.CName
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Order.C

class ArrayTest
  extends hammerlab.Suite {
  val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/matrix")
  test("2-D floats") {
    type T = Float
    type Shape = Int :: Int :: TNil
    type Row[T] = Vector1[T]
    type Arr[T] = Vector2[T]

    implicit val float = DataType.float(LittleEndian)

    val arr: Array[T, Shape, Arr] =
      Array[
        T,
        Shape,
        Arr
      ](
        path
      )
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
    val chunks = rows.map(_.rows.head)

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
        case (actual, expected) ⇒
          ==(actual.start, expected.start)
          ==(actual.end, expected.end)
          ==(actual.idx, expected.idx)
          ==(actual.shape, expected.shape)
      }

    implicit val chunkFold = !![Foldable[Chunk[?, Shape]]]

    val chunkNonzeroCounts =
      chunks
        .map {
          chunkFold.foldLeft(_, 0) {
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
    type T = Long
    type Shape = Int :: TNil
    type Arr[T] = Vector1[T]

    implicit val long = DataType.i64(LittleEndian)

    val arr: Array[T, Shape, Arr] =
      Array[
        T,
        Shape,
        Arr
      ](
        path
      )
      .right
      .get

    val metadata = arr.metadata

    metadata should be(
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

    val chunkPath = path / "0"
    val compressor =
      Blosc(
        cname = CName.lz4,
        clevel = 5,
        shuffle = 1,
        blocksize = 0
      )

    arr.chunks.size should be(1)
    val chunk: Chunk[T, Shape] = arr.chunks(0)
    val bytes = chunk.bytes
    bytes.length should be(223984)

    chunk.size should be(27998)

    implicit val chunkFold = !![Foldable[Chunk[?, Shape]]]

    val nonzeros =
      toFoldableOps[Chunk[?, Shape], Long](chunk).foldLeft(0) {
        (sum, n) ⇒
          if (n > 0)
            sum + 1
          else
            sum
      }

    nonzeros should be(11717)
  }
}
