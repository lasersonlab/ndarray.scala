package org.lasersonlab.zarr

import hammerlab.path._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.io.Load
import org.lasersonlab.zarr.untyped.Struct
import org.lasersonlab.zarr.utils.Idx

class GroupTest
  extends hammerlab.test.Suite
     with HasGetOps
     with Load.syntax
     with zarr.cmp.all {

  implicit val __int: Idx.T[Int] = Idx.Int

  test("load") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.ad.32m.zarr")

    val group @
      Group(
        arrays,
        groups,
        attrs,
        metadata
      ) =
      Group(path) !

    ==(arrays.keySet, Set("X", "obs", "var"))
    ==(groups.keySet, Set("uns"))

    ==(attrs, None)
    ==(metadata, Group.Metadata(`2`))

    val `var`  = group[Struct]('var)
    val `var2` = group ! 'var

    ==(`var`.shape, Dimension(27998) :: Nil)

    val datatype =
      DataType.untyped.Struct(
        List(
          StructEntry(    "index",   long    ),
          StructEntry("Accession", string(18)),
          StructEntry(     "Gene",  short    ),
          StructEntry(   "_LogCV", double    ),
          StructEntry( "_LogMean", double    ),
          StructEntry("_Selected",   long    ),
          StructEntry(   "_Total", double    ),
          StructEntry(   "_Valid",   long    )
        )
      )

    val expected =
      zarr.Metadata(
        shape = List(Dimension(27998)),
        dtype = datatype,
        fill_value =
          Struct(
                "index" → 0L,
            "Accession" → "",
                 "Gene" → 0.toShort,
               "_LogCV" → 0.0,
             "_LogMean" → 0.0,
            "_Selected" → 0L,
               "_Total" → 0.0,
               "_Valid" → 0L
          )
      )

    ==(
      `var`.metadata,
      expected
    )

    ==(
      `var2`
        .metadata
        .as[Struct],  // `var2` was accessed via the "untyped" group.array method
      expected
    )
  }

  import DataType.{ untyped ⇒ _, _ }

  test("typed") {
    import GroupTest._
    import lasersonlab.shapeless.slist._
    val group =
      Foo(
         shorts = Array(10 :: ⊥)((1  to 10).map(_.toShort) : _*),
           ints = Array(10 :: ⊥)( 1  to 10  : _*),
          longs = Array(10 :: ⊥)( 1L to 10L : _*),
         floats = Array(20 :: ⊥)( 0f until 10f  by 0.5f : _*),
        doubles = Array(20 :: ⊥)(0.0 until 10.0 by 0.5  : _*),
        strings = Strings(
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
        )
      )
  }

  test("untyped") {
    val group =
      Group(
        arrays =
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

    ==(group, group2)

    val expected = resource("grouptest.zarr").load[Group[Int]] !

    ==(group, expected)
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
  import lasersonlab.{ zarr ⇒ z }

  case class Foo(
     shorts: z.Array[`1`,  Short],
       ints: z.Array[`1`,    Int],
      longs: z.Array[`1`,   Long],
     floats: z.Array[`1`,  Float],
    doubles: z.Array[`1`, Double],
    strings: Strings
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
