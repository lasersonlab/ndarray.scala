package org.lasersonlab.convert

import java.nio.ByteOrder.LITTLE_ENDIAN
import java.nio.ByteOrder._
import java.nio.ByteBuffer._

import com.sun.jna.{ Memory, NativeLong }
import hammerlab.bytes._
import hammerlab.path._
import io.circe.{ Decoder, Encoder }
import org.blosc.{ IBloscDll, JBlosc }
import org.junit.Assert.{ assertArrayEquals, assertNotNull }
import org.lasersonlab.ndarray.Ints.Ints1
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.FillValue.{ FillValueDecoder, FillValueEncoder }
import org.lasersonlab.zarr.dtype.ByteOrder.LittleEndian
import org.lasersonlab.zarr.dtype.{ ByteOrder, DataType }
import org.lasersonlab.zarr.dtype.DataType.long
import org.lasersonlab.zarr.group.Load
import org.lasersonlab.zarr.{ VectorInts, untyped }
import org.lasersonlab.{ netcdf, zarr }

import scala.collection.mutable.ArrayBuffer

class ConvertTest
  extends hammerlab.test.Suite {
  test("convert") {
    val in = Path("/Users/ryan/c/hdf5-experiments/files/osmFISH_SScortex_mouse_all_cells.h5")
    val group: netcdf.Group = in

    implicit val chunkSize = 32 MB
    implicit val compressor = Blosc()

    val zarrGroup: zarr.untyped.Group = group
    val matrix = zarrGroup.array("matrix")
    val metadata = matrix.metadata

    val out = Path("/Users/ryan/c/hdf5-java-cloud/osmFISH_SScortex_mouse_all_cells.h5.zarr")
//    val out = tmpDir()

    Main.main(in, out)

    ==(
      out
        .list
        .map(_.basename)
        .toSet,
      Set(
        ".zattrs",
        ".zgroup",
        "matrix",
        "layers",
        "col_attrs",
        "row_graphs",
        "row_attrs",
        "col_graphs"
      )
    )
  }

  val expected =
    List[Long](
      2, 12, 10, 13, 6, 5, 8, 10,  1,  9,
      7, 11, 12,  1, 2, 8, 3,  7, 13,  8,
      6,  6, 11,  7, 9, 3, 3, 13,  1, 12,
      2, 10, 5
    )

  test("array") {
    val hdf5Group: netcdf.Group = Path("/Users/ryan/c/hdf5-experiments/files/osmFISH_SScortex_mouse_all_cells.h5")
    val hybridization =
      hdf5Group
        .group('row_attrs)
        .array('Hybridization)

    val it =
      hybridization
        .data
        .getIndexIterator

    val elems = ArrayBuffer[Long]()
    while (it.hasNext) elems += it.getLongNext

    ==(elems.length, 33)
    println(elems.mkString(","))

    implicit val chunkSize = 32 MB
    implicit val compressor = Blosc()

    val zarr: untyped.Array = hybridization
    zarr.elems.toList should be(expected)

    val out = tmpDir()

    {
      import org.lasersonlab.zarr.group.Save.Ops
      zarr.save(out)
    }

    import org.lasersonlab.zarr.group.Load.Ops
    import org.lasersonlab.zarr.Array

    val zarr2 =
      out
        .load[Array.S[Ints1, Long]]
        .right
        .get

    ===(
      zarr2.elems.toList,
      expected
    )
  }

  test("longs") {
    val longs = expected.toArray

    val uncompressedLength = longs.length * 8
    val uncompressedBuffer = allocateDirect(uncompressedLength)
    uncompressedBuffer.order(LITTLE_ENDIAN)
    longs.foreach {uncompressedBuffer.putLong }
    uncompressedBuffer.position(0)

    val compressedBufferSize = uncompressedLength + JBlosc.OVERHEAD
    val compressedBuffer = allocateDirect(compressedBufferSize)

    val numCompressed = JBlosc.compressCtx(5, 1, 8, uncompressedBuffer, uncompressedLength, compressedBuffer, compressedBufferSize, "lz4", 0, 1)
    numCompressed should be(73)

    val numDecompressed = JBlosc.decompressCtx(compressedBuffer, uncompressedBuffer, uncompressedLength, 1)
    numDecompressed should be(uncompressedLength)

    uncompressedBuffer.position() should be(0)
    for {
      (l, i) ← longs.zipWithIndex
    } withClue(s"index $i: ") {
      uncompressedBuffer.getLong should be(l)
    }

    val fileBytes = Path("/Users/ryan/c/hdf5-experiments/files/osmFISH_SScortex_mouse_all_cells.h5.zarr/row_attrs/Hybridization/0").readBytes
    fileBytes.length should be(numCompressed)

    val destSlice = Array.fill[Byte](fileBytes.length)(0)
    compressedBuffer.get(destSlice)
    destSlice should be(fileBytes)

    val fileBuffer = allocateDirect(fileBytes.length)
    fileBuffer.put(fileBytes)

    val decompressedFile = JBlosc.decompressCtx(compressedBuffer, uncompressedBuffer, uncompressedLength, 1)
    decompressedFile should be(uncompressedLength)

    uncompressedBuffer.position() should be(0)
    for {
      (l, i) ← longs.zipWithIndex
    } withClue(s"index $i: ") {
      uncompressedBuffer.getLong should be(l)
    }

    import java.nio.ByteOrder._
    uncompressedBuffer.position(0)
    val dtype = long(LittleEndian)
    uncompressedBuffer.order() should be(LITTLE_ENDIAN)
    uncompressedBuffer.getLong should be(2)
  }

  test("blosc") {
    val SIZE = 100 * 100 * 100
    val data = new Array[Float](SIZE)
    var i = 0
    while (i < SIZE) {
      data(i) = i * 2
      i += 1
    }
    var data_out = new Array[Float](SIZE)
    val isize = SIZE * 4
    val m = new Memory(isize)
    m.write(0, data, 0, SIZE)
    val m2 = new Memory(isize)
    IBloscDll.blosc_init()
    val size = IBloscDll.blosc_compress(5, 1, new NativeLong(4), new NativeLong(isize), m, m2, new NativeLong(isize))
    data_out = m2.getFloatArray(0, SIZE)
    val m3 = new Memory(isize)
    IBloscDll.blosc_decompress(m2, m3, new NativeLong(isize))
    val data_in = m3.getFloatArray(0, SIZE)
    assertArrayEquals(data, data_in, 0.toFloat)
    IBloscDll.blosc_destroy()
    assertNotNull(data_out)
    assert(size < isize)
  }
}
