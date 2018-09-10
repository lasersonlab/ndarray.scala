package org.lasersonlab.zarr

import _root_.io.circe.DecodingFailure
import _root_.io.circe.parser.decode
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray.Ints._

class TListDecoderTest
  extends hammerlab.Suite {
  test("decode") {
    decode[TNil]("""[]""") should be(Right(TNil))
    decode[TNil]("""[1]""") should be(Left(DecodingFailure("Found extra elements: 1", Nil)))

    decode[Ints1]("""[1]""") should be(Right(1 :: TNil))
    decode[Ints1]("""[]""") should be(Left(DecodingFailure("Too few elements", Nil)))
    decode[Ints1]("""[1,2]""") should be(Left(DecodingFailure("Found extra elements: 2", Nil)))

    decode[Ints2]("""[1,2]""") should be(Right(1 :: 2 :: TNil))
    decode[Ints2]("""[]""") should be(Left(DecodingFailure("Too few elements", Nil)))
    decode[Ints2]("""[1]""") should be(Left(DecodingFailure("Too few elements", Nil)))
    decode[Ints2]("""[1,2,3]""") should be(Left(DecodingFailure("Found extra elements: 3", Nil)))
    decode[Ints2]("""[1,2,3,4]""") should be(Left(DecodingFailure("Found extra elements: 3,4", Nil)))
  }
}
