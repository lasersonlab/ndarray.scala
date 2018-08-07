package org.lasersonlab.zarr

import hammerlab.shapeless.tlist._
import io.circe.DecodingFailure
import io.circe.parser.decode

class TListDecoderTest
  extends hammerlab.Suite {
  test("decode") {
    decode[TNil]("""[]""") should be(Right(TNil))
    decode[TNil]("""[1]""") should be(Left(DecodingFailure("Found extra elements: 1", Nil)))

    decode[Int :: TNil]("""[1]""") should be(Right(1 :: TNil))
    decode[Int :: TNil]("""[]""") should be(Left(DecodingFailure("Too few elements", Nil)))
    decode[Int :: TNil]("""[1,2]""") should be(Left(DecodingFailure("Found extra elements: 2", Nil)))

    decode[Int :: Int :: TNil]("""[1,2]""") should be(Right(1 :: 2 :: TNil))
    decode[Int :: Int :: TNil]("""[]""") should be(Left(DecodingFailure("Too few elements", Nil)))
    decode[Int :: Int :: TNil]("""[1]""") should be(Left(DecodingFailure("Too few elements", Nil)))
    decode[Int :: Int :: TNil]("""[1,2,3]""") should be(Left(DecodingFailure("Found extra elements: 3", Nil)))
    decode[Int :: Int :: TNil]("""[1,2,3,4]""") should be(Left(DecodingFailure("Found extra elements: 3,4", Nil)))
  }
}
