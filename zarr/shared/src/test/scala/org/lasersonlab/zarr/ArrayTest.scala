package org.lasersonlab.zarr

import hammerlab.option
import hammerlab.option.Opt
import hammerlab.path._
import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.{ Decoder, HCursor }
import org.lasersonlab.zarr.ByteOrder.LittleEndian
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Compressor.Blosc.CName.lz4
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Order.C

class ArrayTest
  extends hammerlab.Suite {

  implicit def optDecoder[T: Decoder]: Decoder[Opt[T]] =
    new Decoder[option.Opt[T]] {
      override def apply(c: HCursor): Result[Opt[T]] =
        Decoder
          .decodeOption[T]
          .apply(c)
          .map { o ⇒ o: Opt[T] }
    }

  test("read") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/matrix/.zarray")

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
