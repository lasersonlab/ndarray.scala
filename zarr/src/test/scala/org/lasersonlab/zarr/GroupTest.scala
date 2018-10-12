package org.lasersonlab.zarr

import hammerlab.path._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.io.{ Load, Save }
import org.lasersonlab.zarr.untyped.Struct
import org.lasersonlab.zarr.utils.Idx
import shapeless.labelled.FieldType
import shapeless.{ HNil, LabelledGeneric, Witness }
import lasersonlab.{ zarr ⇒ z }

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
        ),
        structs = Structs(
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
                     short =  n.toShort,
                       int =  n,
                      long = n          * 1e9 toLong,
                     float = n.toFloat  *  10,
                    double = n.toDouble * 100
                  )
              )
              : _*
            )
        )
      )

//    !![Save[Strings]]
//    !![Save[Bar]]
//    !![Save[Structs]]
//
//    val actual = tmpDir()
//    group.save(actual).!

//    val group2 = actual.load[Foo] !
//
//    ==(group, group2)
//
//    val expected = resource("grouptest.zarr").load[Foo] !
//
//    ==(group, expected)
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

  case class Foo(
     shorts: z.Array[`1`,  Short],
       ints: z.Array[`1`,    Int],
      longs: z.Array[`1`,   Long],
     floats: z.Array[`1`,  Float],
    doubles: z.Array[`1`, Double],
    strings: Strings,
    structs: Structs
  )

  case class Bar(strings: Strings)

  case class Strings(
    s2: z.Array[`1`, String],
    s3: z.Array[`2`, String]
  )

  case class Structs(
       ints: z.Array[`1`,      I4],
    numbers: z.Array[`2`, Numbers]
  )
}

object Test {
//  import shapeless._
//  implicit val int: Save[Int] = ???
//  case class A(n: Int)
//  case class B(a: A)
//  the[Save[A]]
//  the[Save[B]]
//
//  import lasersonlab.shapeless.slist._
//
//  case class C(arr: z.Array[`1`, Int])
//  case class D(c: C)
//
//  the[Save[z.Array[`1`, Int]]]
//  the[Lazy[Save[z.Array[`1`, Int]]]]
//  the[Save[C]]
//  the[Save[D]]
  import shapeless._, labelled.FieldType

  trait LL[T]  // typeclass with Lazy LabelledGeneric derivations
  object LL {
    implicit val hnilLL: LL[HNil] = ???
    implicit def consLL[K <: Symbol, H, T <: HList](implicit w: Witness.Aux[K], h: Lazy[LL[H]], t: Lazy[LL[T]]): LL[FieldType[K, H] :: T] = ???
    implicit def ccLL[CC, L <: HList](implicit g: LabelledGeneric.Aux[CC, L], l: Lazy[LL[L]]): LL[CC] = ???
  }

  trait LG[T]  // typeclass with Lazy Generic derivations
  object LG {
    implicit val hnilLG: LG[HNil] = ???
    implicit def consLG[H, T <: HList](implicit h: Lazy[LG[H]], t: Lazy[LG[T]]): LG[H :: T] = ???
    implicit def ccLG[CC, L <: HList](implicit g: Generic.Aux[CC, L], l: Lazy[LG[L]]): LG[CC] = ???
  }

  trait EL[T]  // typeclass with non-Lazy LabelledGeneric derivations
  object EL {
    implicit val hnilEL: EL[HNil] = ???
    implicit def consEL[K <: Symbol, H, T <: HList](implicit w: Witness.Aux[K], h: EL[H], t: EL[T]): EL[FieldType[K, H] :: T] = ???
    implicit def ccEL[CC, L <: HList](implicit g: LabelledGeneric.Aux[CC, L], l: EL[L]): EL[CC] = ???
  }

  trait EG[T]  // typeclass with non-Lazy Generic derivations
  object EG {
    implicit val hnilEG: EG[HNil] = ???
    implicit def consEG[H, T <: HList](implicit h: EG[H], t: EG[T]): EG[H :: T] = ???
    implicit def ccEG[CC, L <: HList](implicit g: Generic.Aux[CC, L], l: EG[L]): EG[CC] = ???
  }

  // tests on a type with a simple type-member:
   trait A { type T }
  object A { type Aux[_T] = A { type T = _T } }  // "Aux" alias
  case class A1[T](a1: A.Aux[T], a2: A.Aux[T])
  case class A2[T](a1: A1   [T])

  implicit def a1[T]: LL[A.Aux[T]] = ???
  implicit def a2[T]: LG[A.Aux[T]] = ???
  implicit def a3[T]: EL[A.Aux[T]] = ???
  implicit def a4[T]: EG[A.Aux[T]] = ???

  the[     LL[A.Aux[Int] ]]  // ✅
  the[     LG[A.Aux[Int] ]]  // ✅
  the[     EL[A.Aux[Int] ]]  // ✅
  the[     EG[A.Aux[Int] ]]  // ✅

  the[Lazy[LL[A.Aux[Int]]]]  // ✅
  the[Lazy[LG[A.Aux[Int]]]]  // ✅
  the[Lazy[EG[A.Aux[Int]]]]  // ✅
  the[Lazy[EL[A.Aux[Int]]]]  // ✅

  the[     LL[A1   [Int] ]]  // 🚫
  the[     LG[A1   [Int] ]]  // ✅
  the[     EL[A1   [Int] ]]  // ✅
  the[     EG[A1   [Int] ]]  // ✅

  the[     LL[A2   [Int] ]]  // 🚫
  the[     LG[A2   [Int] ]]  // ✅
  the[     EG[A2   [Int] ]]  // 🚫 this is expected: non-lazy derivation
  the[     EL[A2   [Int] ]]  // 🚫 this is expected: non-lazy derivation

  // tests on a type with a an HKT-member:
   trait B { type T[_] }
  object B { type Aux[_T[_]] = B { type T[U] = _T[U] } }  // "Aux" alias
  case class B1[T[_]](b1: B.Aux[T], b2: B.Aux[T])
  case class B2[T[_]](b1: B1   [T])

  implicit def b1[T[_]]: LL[B.Aux[T]] = ???
  implicit def b2[T[_]]: EL[B.Aux[T]] = ???
  implicit def b3[T[_]]: LG[B.Aux[T]] = ???
  implicit def b4[T[_]]: EG[B.Aux[T]] = ???

  the[     LL[B.Aux[List]] ]  // ✅
  the[     LG[B.Aux[List]] ]  // ✅
  the[     EL[B.Aux[List]] ]  // ✅
  the[     EG[B.Aux[List]] ]  // ✅

  the[Lazy[LL[B.Aux[List]]]]  // 🚫
  the[Lazy[LG[B.Aux[List]]]]  // 🚫
  the[Lazy[EL[B.Aux[List]]]]  // 🚫
  the[Lazy[EG[B.Aux[List]]]]  // 🚫

  the[     LL[B1   [List]] ]  // 🚫
  the[     LG[B1   [List]] ]  // 🚫
  the[     EL[B1   [List]] ]  // ✅
  the[     EG[B1   [List]] ]  // ✅

  the[     LL[B2   [List]] ]  // 🚫
  the[     LG[B2   [List]] ]  // 🚫
  the[     EL[B2   [List]] ]  // 🚫
  the[     EG[B2   [List]] ]  // 🚫
}
