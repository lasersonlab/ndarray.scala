package org.lasersonlab.zarr

import java.util.Random

import hammerlab.scalajs._
import lasersonlab.{ zarr ⇒ z }
import org.lasersonlab.test.future.Assert
import org.lasersonlab.uri.Local
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Compressor.{ Blosc, ZLib }
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.Load
import utest._

object GroupTest
  extends lasersonlab.Suite
     with Load.syntax
     with zarr.cmp.all
     with Assert.syntax
{
  import DataType._

  // set this to `true` to overwrite the existing "expected" data in src/test/resources
  val writeNewExpectedData = false

  val resourceDirectory =
    (
      Local.cwd.parent js_?
      (Local.cwd / "zarr")
    ) / "shared" / "src" / "test" / "resources"

  override lazy val resourceDirectories: List[Local] = resourceDirectory :: Nil

  val bytes =
    for {
      x ← 1 to  50
      y ← 1 to 100
      z ← 1 to 200
    } yield (
      if (y % 2 == 0) '0' else
      if (z % 3 == 0) 'A' else
      if (x % 2 == 0) 'z' else
                      ' '
    )
    .toByte

  import math.{ exp, sqrt }

  val random = new Random(1729)
  import random.{ nextGaussian ⇒ unif }

  // Generate random (log-normal) Doubles, and repeat each one ceil(χ²) number of times
  lazy val stream: Stream[Double] = {
    val a = unif
    val b = unif
    val repetitions = (a * a).ceil.toInt
    val elem = exp(b)
    Stream.fill(repetitions)(elem) #::: stream
  }

  val  shorts = (1  to 16).map(_.toShort)
  val    ints = (1  to 1000).map(sqrt(_).toInt ).zipWithIndex.map { case (n, i) ⇒ if (i % 2 == 0) n else -n }
  val   longs = ints.map(_.toLong)
  val  floats = stream.map(_.toFloat).take(10000)
  val doubles = stream.take(20)

  val i4s = (1 to 10).map { I4 }
  val numbers =
    for {
      r ← 0 until 10
      c ← 0 until 10
      n = 10 * r + c
    } yield
      Numbers(
         short = n.toShort,
           int = n,
          long = n          * 1e9 toLong,
         float = n.toFloat  *  10,
        double = n.toDouble * 100
      )

  val `grouptest.zarr` = resourceDirectory / ("grouptest.zarr" js_? "grouptest-zlib.zarr")
  implicit val compressor = Blosc() js_? ZLib()

  import scala.Array  // needed until next release of utest: https://github.com/lihaoyi/utest/pull/186/files#r247263418
  val tests = Tests {
    'typed - {
      import lasersonlab.zarr.Array
      val group =
        Foo(
            bytes = Array(       50 :: 100 :: 200 :: ⊥,      20 :: 50 :: 110 :: ⊥)(   bytes ),
           shorts = Array( 2 ::   2 ::   2 ::   2 :: ⊥, 1 ::  1 ::  1 ::   1 :: ⊥)(  shorts ),
             ints = Array(                   1000 :: ⊥                           )(    ints ),
            longs = Array(                   1000 :: ⊥,                  100 :: ⊥)(   longs ),
           floats = Array(             100 :: 100 :: ⊥,            20 :: 100 :: ⊥)(  floats ),
          doubles = Array(                     20 :: ⊥                           )( doubles ),
          strings =
            Strings(
              s2 = {
                // TODO: add optional path for inferring string datatype size from data (requires O(n) pass)
                implicit val d = string(2)
                Array(10 :: ⊥)((1 to 10).map(_.toString))
              },
              s3 = {
                implicit val d = string(3)
                Array(
                  10 :: 10 :: ⊥,
                   3 ::  4 :: ⊥
                )(
                  (1 to 100).map(_.toString)
                )
              }
            ),
          structs =
            Structs(
                 ints = Array(      10 :: ⊥              )(    i4s),
              numbers = Array(10 :: 10 :: ⊥, 2 ::  5 :: ⊥)(numbers)
            )
        )

      if (writeNewExpectedData)
        group.save(`grouptest.zarr`)
      else {
        val actual = tmpPath()

        for {
          _ ← group.save(actual)
          _ ← ==(actual, `grouptest.zarr`)
          group2 ← actual.load[Foo]
          _ ← ==(group, group2)
          expected ← `grouptest.zarr`.load[Foo]
          _ ← ==(group, expected)
        } yield
          ()
      }
    }

//    "typed – mixed compressors / endianness" - {
//      import lasersonlab.zarr.Array
//      import org.lasersonlab.zarr.Compressor.Blosc
//      import org.lasersonlab.zarr.Compressor.Blosc.{ == ⇒ _, _ }
//
//      val group =
//        Foo(
//            bytes = { Array(       50 :: 100 :: 200 :: ⊥,      20 :: 50 :: 110 :: ⊥,  compressor = z.compress.zlib )(   bytes ) },
//           shorts = { Array( 2 ::   2 ::   2 ::   2 :: ⊥, 1 ::  1 ::  1 ::   1 :: ⊥,  compressor = z.compress.   - )(  shorts ) },
//             ints = { Array(                   1000 :: ⊥,                             compressor =   Blosc( lz4hc) )(    ints ) },
//            longs = { Array(                   1000 :: ⊥,                   100 :: ⊥, compressor =   Blosc(  zlib) )(   longs ) },
//           floats = { Array(             100 :: 100 :: ⊥,             20 :: 100 :: ⊥, compressor =   Blosc(  zstd) )(  floats ) },
//          doubles = { Array(                     20 :: ⊥,                             compressor =   Blosc(snappy) )( doubles ) },
//          strings =
//            Strings(
//              s2 = {
//                implicit val d = string(2)
//                Array(10 :: ⊥)((1 to 10).map(_.toString))
//              },
//              s3 = {
//                implicit val d = string(3)
//                Array(
//                  10 :: 10 :: ⊥,
//                   3 ::  4 :: ⊥
//                )(
//                  (1 to 100).map(_.toString)
//                )
//              }
//            ),
//          structs =
//            Structs(
//              ints = {
//                implicit val > = z.order.>
//                Array(10 :: ⊥)(i4s)
//              },
//              numbers = {
//                implicit val short_> = I16(z.order.>)
//                implicit val  long_> = I64(z.order.>)
//                Array(
//                  10 :: 10 :: ⊥,
//                   2 ::  5 :: ⊥
//                )(
//                  numbers
//                )
//              }
//            )
//        )
//
//      if (writeNewExpectedData)
//        group.save(Path("mixed-gzip.zarr"))
//      else {
//        val actual = tmpPath()
//        val expectedPath = resource("mixed-gzip.zarr")
//        for {
//          _ ← group.save(actual)
//          _ ← ==(actual, expectedPath)
//          group2 ← actual.load[Foo]
//          _ ← ==(group, group2)
//          expected ← expectedPath.load[Foo]
//          _ ← ==(group, expected)
//        } yield
//          ()
//      }
//    }

    'untyped - {
      import z.Array
      val group =
        Group(
            'bytes → Array(       50 :: 100 :: 200 :: Nil,      20 :: 50 :: 110 :: Nil)(   bytes ),
           'shorts → Array( 2 ::   2 ::   2 ::   2 :: Nil, 1 ::  1 ::  1 ::   1 :: Nil)(  shorts ),
             'ints → Array(                   1000 :: Nil                             )(    ints ),
            'longs → Array(                   1000 :: Nil,                  100 :: Nil)(   longs ),
           'floats → Array(             100 :: 100 :: Nil,            20 :: 100 :: Nil)(  floats ),
          'doubles → Array(                     20 :: Nil                             )( doubles ),
          'strings →
            Group(
              's2 → {
                implicit val d = string(2)
                Array(10 :: Nil)((1 to 10).map(_.toString))
              },
              's3 → {
                implicit val d = string(3)
                Array(
                  10 :: 10 :: Nil,
                   3 ::  4 :: Nil
                )(
                  (1 to 100).map(_.toString)
                )
              }
            ),
          'structs →
            Group(
              'ints → {
                implicit val dtype = struct.?('value → int)
                Array(10 :: Nil)(
                  (1 to 10)
                    .map {
                      i ⇒
                        untyped.Struct("value" → i)
                    }

                )
              },
              'numbers → {
                implicit val dtype =
                  struct.?(
                     'short → short,
                       'int → int,
                      'long → long,
                     'float → float,
                    'double → double
                  )
                Array(
                  10 :: 10 :: Nil,
                   2 ::  5 :: Nil
                )(
                  for {
                    r ← 0 until 10
                    c ← 0 until 10
                    n = 10 * r + c
                  } yield
                    untyped.Struct(
                       "short" →  n.toShort,
                         "int" →  n,
                        "long" → (n          * 1e9 toLong),
                       "float" → (n.toFloat  *  10),
                      "double" → (n.toDouble * 100)
                    )
                )
              }
            )
        )

      if (!writeNewExpectedData) {
        val actual = tmpPath()

        for {
          _ ← group.save(actual)
          group2 ← actual.load[Group[Int]]
          _ ← ==(group, group2)
          expected ← `grouptest.zarr`.load[Group[Int]]
          _ ← ==(group, expected)
        } yield
          ()
      }
    }
  }

/**
 * Sample record-types for testing IO above
 */
//object GroupTest {
  case class I4(value: Int)
  case class Numbers(
     short: Short,
       int: Int,
      long: Long,
     float: Float,
    double: Double
  )

  case class Foo(
      bytes: z.Array[`3`,   Byte],
     shorts: z.Array[`4`,  Short],
       ints: z.Array[`1`,    Int],
      longs: z.Array[`1`,   Long],
     floats: z.Array[`2`,  Float],
    doubles: z.Array[`1`, Double],
    strings: Strings,
    structs: Structs
  )

  case class Strings(
    s2: z.Array[`1`, String],
    s3: z.Array[`2`, String]
  )

  case class Structs(
       ints: z.Array[`1`,      I4],
    numbers: z.Array[`2`, Numbers]
  )
}
