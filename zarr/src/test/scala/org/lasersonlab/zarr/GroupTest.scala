package org.lasersonlab.zarr

import lasersonlab.{ zarr ⇒ z }
import org.lasersonlab.zarr
import org.lasersonlab.zarr.cmp.Cmp
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.Load
import org.lasersonlab.zarr.utils.Idx

class GroupTest
  extends hammerlab.test.Suite
     with HasGetOps
     with Load.syntax
     with zarr.cmp.all
     with Cmp.syntax {

  implicit val __int: Idx.T[Int] = Idx.Int

  import DataType.{ untyped ⇒ _, _ }

  test("typed") {
    import GroupTest.{ == ⇒ _, _ }
    import lasersonlab.shapeless.slist.{ == ⇒ _, _ }

    val group =
      Foo(
         shorts = Array(10 :: ⊥)((1  to 10).map(_.toShort) : _*),
           ints = Array(10 :: ⊥)( 1  to 10  : _*),
          longs = Array(10 :: ⊥)( 1L to 10L : _*),
         floats = Array(20 :: ⊥)( 0f until 10f  by 0.5f : _*),
        doubles = Array(20 :: ⊥)(0.0 until 10.0 by 0.5  : _*),
        strings =
          Strings(
            s2 = {
              implicit val d = string(2)
              Array(10 :: ⊥)((1 to 10).map(_.toString): _*)
            },
            s3 = {
              implicit val d = string(3)
              Array(
                10 :: 10 :: ⊥,
                 3 ::  4 :: ⊥
              )(
                (1 to 100).map(_.toString): _*
              )
            }
          ),
        structs =
          Structs(
            ints =
              Array(10 :: ⊥)(
                (1 to 10).map { I4 }: _*
              ),
            numbers =
              Array(
                10 :: 10 :: ⊥,
                 2 ::  5 :: ⊥
              )(
                (
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
                )
                : _*
              )
          )
      )

    val actual = tmpDir()
    group.save(actual).!

    val group2 = actual.load[Foo] !

    eqv(group, group2)

    val expected = resource("grouptest.zarr").load[Foo] !

    eqv(group, expected)
  }

  test("untyped") {
    val group =
      Group(
        arrays =
          // TODO: remove need for explicit types on this Map
          Map[String, Array.??[Int]](
             "shorts" → Array(10 :: Nil)((1  to 10).map(_.toShort) : _*),
               "ints" → Array(10 :: Nil)( 1  to 10                 : _*),
              "longs" → Array(10 :: Nil)( 1L to 10L                : _*),
             "floats" → Array(20 :: Nil)( 0f until 10f  by 0.5f    : _*),
            "doubles" → Array(20 :: Nil)(0.0 until 10.0 by 0.5     : _*)
          ),
        groups =
          Map(
            "strings" →
              Group(
                Map[String, Array.??[Int]](
                  "s2" → {
                    implicit val d = string(2)
                    Array(10 :: Nil)((1 to 10).map(_.toString): _*)
                  },
                  "s3" → {
                    implicit val d = string(3)
                    Array(
                      10 :: 10 :: Nil,
                       3 ::  4 :: Nil
                    )(
                      (1 to 100).map(_.toString): _*
                    )
                  }
                )
              ),
            "structs" →
              Group(
                Map[String, Array.??[Int]](
                  "ints" → {
                    implicit val datatype =
                      DataType.untyped.Struct(
                        StructEntry("value", int) :: Nil
                      )
                    Array(10 :: Nil)(
                      (1 to 10)
                        .map {
                          i ⇒
                            untyped.Struct("value" → i)
                        }
                      : _*
                    )
                  },
                  "numbers" → {
                    implicit val datatype =
                      DataType.untyped.Struct(
                        List(
                          StructEntry( "short",  short),
                          StructEntry(   "int",    int),
                          StructEntry(  "long",   long),
                          StructEntry( "float",  float),
                          StructEntry("double", double)
                        )
                      )
                    Array(
                      10 :: 10 :: Nil,
                       2 ::  5 :: Nil
                    )(
                      (
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
                      : _*
                    )
                  }
                )
              )
          )
      )

    val actual = tmpDir()
    group.save(actual).!

    val group2 = actual.load[Group[Int]] !

    eqv(group, group2)

    val expected = resource("grouptest.zarr").load[Group[Int]] !

    eqv(group, expected)
  }
}

object GroupTest {
  case class I4(value: Int)
  case class Numbers(
     short: Short,
       int: Int,
      long: Long,
     float: Float,
    double: Double
  )

  import lasersonlab.shapeless.slist._

  case class Foo(
     shorts: z.Array[`1`,  Short],
       ints: z.Array[`1`,    Int],
      longs: z.Array[`1`,   Long],
     floats: z.Array[`1`,  Float],
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
