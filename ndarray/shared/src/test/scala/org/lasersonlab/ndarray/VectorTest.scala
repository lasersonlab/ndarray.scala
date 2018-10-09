package org.lasersonlab.ndarray

import cats.implicits._
import lasersonlab.shapeless.slist._
import org.lasersonlab.ndarray.Vector.{ Arg, IsArg }

class VectorTest
  extends hammerlab.Suite {

  def elem(elems: Int*) = elems.mkString(",")

  test("1-D") {
    for {
      n ← 1 to 10
      shape = n :: ⊥
      elems = (0 until n).map(elem(_))
      vector = Vector(shape, elems: _*)
    } withClue(s"shape ($n): ") {

      intercept[IllegalArgumentException] { Vector(shape, elems.dropRight(1): _*) }
      intercept[IllegalArgumentException] { Vector(shape, elems.dropRight(2): _*) }
      intercept[IllegalArgumentException] { Vector(shape                        ) }
      intercept[IllegalArgumentException] { Vector(shape, elems :+ ""       : _*) }
      intercept[IllegalArgumentException] { Vector(shape, elems :+ "" :+ "" : _*) }

      ==(vector.shape, shape)
      ==(vector.toList, elems.toList)

      for { i ← 0 until n } { ==(vector(i :: ⊥), elem(i)) }

      def oob(i: Int): Unit =
        withClue(s"index $i: ") {
          intercept[IndexOutOfBoundsException] { vector(i :: ⊥) }
        }

      oob(     n)
      oob( n + 1)
      oob(-    1)
      oob(-   10)
    }
  }

  test("1-D manual") {
    val `1` = Vector(1 :: ⊥, "111")
    val `2` = Vector(2 :: ⊥, "111", "222")
    val `3` = Vector(3 :: ⊥, "111", "222", "333")

    ==(`1`.shape, 1 :: ⊥)
    ==(`2`.shape, 2 :: ⊥)
    ==(`3`.shape, 3 :: ⊥)

    ==(`1`.toList, List("111"))
    ==(`2`.toList, List("111", "222"))
    ==(`3`.toList, List("111", "222", "333"))

    ==(`1`(0 :: ⊥), "111")
    intercept[IndexOutOfBoundsException] { `1`(1 :: ⊥) }

    ==(`2`(0 :: ⊥), "111")
    ==(`2`(1 :: ⊥), "222")
    intercept[IndexOutOfBoundsException] { `2`(2 :: ⊥) }

    ==(`3`(0 :: ⊥), "111")
    ==(`3`(1 :: ⊥), "222")
    ==(`3`(2 :: ⊥), "333")
    intercept[IndexOutOfBoundsException] { `3`(3 :: ⊥) }

    withClue("too few args: ") {
      intercept[IllegalArgumentException] { Vector(1 :: ⊥) }

      intercept[IllegalArgumentException] { Vector(2 :: ⊥) }
      intercept[IllegalArgumentException] { Vector(2 :: ⊥, "111") }

      intercept[IllegalArgumentException] { Vector(3 :: ⊥) }
      intercept[IllegalArgumentException] { Vector(3 :: ⊥, "111") }
      intercept[IllegalArgumentException] { Vector(3 :: ⊥, "111", "222") }
    }

    withClue("too many args: ") {
      intercept[IllegalArgumentException] { Vector(1 :: ⊥, "111", "222") }
      intercept[IllegalArgumentException] { Vector(1 :: ⊥, "111", "222", "333") }

      intercept[IllegalArgumentException] { Vector(2 :: ⊥, "111", "222", "333") }
      intercept[IllegalArgumentException] { Vector(2 :: ⊥, "111", "222", "333", "444") }

      intercept[IllegalArgumentException] { Vector(3 :: ⊥, "111", "222", "333", "444") }
      intercept[IllegalArgumentException] { Vector(3 :: ⊥, "111", "222", "333", "444", "555") }
    }
  }

  test("2-D") {
    for {
      m ← 1 to 6
      n ← 1 to 6
      shape = m :: n :: ⊥
    } withClue(s"${m}x$n: ") {
      val elems =
        (
          for {
            r ← 0 until m
            c ← 0 until n
          } yield
            elem(r, c)
        )
        .toVector

      val vector = Vector(shape, elems: _*)

      intercept[IllegalArgumentException] { Vector(shape, elems.dropRight(1): _*) }
      intercept[IllegalArgumentException] { Vector(shape, elems.dropRight(2): _*) }
      intercept[IllegalArgumentException] { Vector(shape                        ) }
      intercept[IllegalArgumentException] { Vector(shape, elems :+ ""       : _*) }
      intercept[IllegalArgumentException] { Vector(shape, elems :+ "" :+ "" : _*) }

      ==(vector.shape, shape)
      ==(vector.toList, elems.toList)

      for {
        r ← 0 until m
        c ← 0 until n
      } {
        ==(vector(r :: c :: ⊥), elem(r, c))
      }

      def oob(r: Int, c: Int): Unit =
        withClue(s"($r,$c): ") {
          intercept[IndexOutOfBoundsException] { vector(r :: c :: ⊥) }
        }

      for { c ← 0 until n } {
        oob( m + 1,      c)
        oob(     m,      c)
        oob(-    1,      c)
        oob(-   10,      c)
      }
      for { r ← 0 until m } {
        oob(     r,  n + 1)
        oob(     r,      n)
        oob(     r, -    1)
        oob(     r, -   10)
      }
    }
  }

  test("2-D manual") {
    val `1x1` = Vector(1 :: 1 :: ⊥, "0,0")
    val `1x2` = Vector(1 :: 2 :: ⊥, "0,0", "0,1")
    val `2x1` = Vector(2 :: 1 :: ⊥, "0,0", "1,0")
    val `2x2` = Vector(2 :: 2 :: ⊥, "0,0", "0,1", "1,0", "1,1")
    val `1x3` = Vector(1 :: 3 :: ⊥, "0,0", "0,1", "0,2")
    val `3x1` = Vector(3 :: 1 :: ⊥, "0,0", "1,0", "2,0")

    ==(`1x1`.shape, 1 :: 1 :: ⊥) ; ==(`1x2`.shape, 1 :: 2 :: ⊥) ; ==(`1x3`.shape, 1 :: 3 :: ⊥)
    ==(`2x1`.shape, 2 :: 1 :: ⊥) ; ==(`2x2`.shape, 2 :: 2 :: ⊥)
    ==(`3x1`.shape, 3 :: 1 :: ⊥)

    ==(`1x1`.toList, List("0,0"))
    ==(`1x2`.toList, List("0,0", "0,1"))
    ==(`2x1`.toList, List("0,0", "1,0"))
    ==(`2x2`.toList, List("0,0", "0,1", "1,0", "1,1"))
    ==(`1x3`.toList, List("0,0", "0,1", "0,2"))
    ==(`3x1`.toList, List("0,0", "1,0", "2,0"))

    ==(`1x1`(0 :: 0 :: ⊥), "0,0")
    intercept[IndexOutOfBoundsException] { `1x1`(1 :: 0 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `1x1`(0 :: 1 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `1x1`(1 :: 1 :: ⊥) }

    ==(`1x2`(0 :: 0 :: ⊥), "0,0")
    ==(`1x2`(0 :: 1 :: ⊥), "0,1")
    intercept[IndexOutOfBoundsException] { `1x2`(1 :: 0 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `1x2`(0 :: 2 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `1x2`(1 :: 2 :: ⊥) }

    ==(`2x1`(0 :: 0 :: ⊥), "0,0")
    ==(`2x1`(1 :: 0 :: ⊥), "1,0")
    intercept[IndexOutOfBoundsException] { `2x1`(2 :: 0 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `2x1`(0 :: 1 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `2x1`(2 :: 1 :: ⊥) }

    ==(`2x2`(0 :: 0 :: ⊥), "0,0")
    ==(`2x2`(0 :: 1 :: ⊥), "0,1")
    ==(`2x2`(1 :: 0 :: ⊥), "1,0")
    ==(`2x2`(1 :: 1 :: ⊥), "1,1")
    intercept[IndexOutOfBoundsException] { `2x2`(2 :: 0 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `2x2`(0 :: 2 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `2x2`(2 :: 2 :: ⊥) }

    ==(`1x3`(0 :: 0 :: ⊥), "0,0")
    ==(`1x3`(0 :: 1 :: ⊥), "0,1")
    ==(`1x3`(0 :: 2 :: ⊥), "0,2")
    intercept[IndexOutOfBoundsException] { `1x3`(1 :: 0 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `1x3`(0 :: 3 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `1x3`(1 :: 3 :: ⊥) }

    ==(`3x1`(0 :: 0 :: ⊥), "0,0")
    ==(`3x1`(1 :: 0 :: ⊥), "1,0")
    ==(`3x1`(2 :: 0 :: ⊥), "2,0")
    intercept[IndexOutOfBoundsException] { `3x1`(0 :: 1 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `3x1`(3 :: 0 :: ⊥) }
    intercept[IndexOutOfBoundsException] { `3x1`(3 :: 1 :: ⊥) }
  }

  test("3-D") {
    for {
      m ← 1 to 6
      n ← 1 to 6
      o ← 1 to 6
      shape = m :: n :: o :: ⊥
    } withClue(s"${m}x${n}x$o: ") {
      val elems =
        (
          for {
            x ← 0 until m
            y ← 0 until n
            z ← 0 until o
          } yield
            elem(x, y, z)
          )
          .toVector

      val vector = Vector(shape, elems: _*)

      intercept[IllegalArgumentException] { Vector(shape, elems.dropRight(1): _*) }
      intercept[IllegalArgumentException] { Vector(shape, elems.dropRight(2): _*) }
      intercept[IllegalArgumentException] { Vector(shape                        ) }
      intercept[IllegalArgumentException] { Vector(shape, elems :+ ""       : _*) }
      intercept[IllegalArgumentException] { Vector(shape, elems :+ "" :+ "" : _*) }

      ==(vector.shape, shape)
      ==(vector.toList, elems.toList)

      for {
        x ← 0 until m
        y ← 0 until n
        z ← 0 until o
      } {
        ==(vector(x :: y :: z :: ⊥), elem(x, y, z))
      }

      def oob(x: Int, y: Int, z: Int): Unit =
        withClue(s"($x,$y,$z): ") {
          intercept[IndexOutOfBoundsException] { vector(x :: y :: z :: ⊥) }
        }

      for {
        y ← 0 until n
        z ← 0 until o
      } {
        oob( m + 1,      y,      z)
        oob(     m,      y,      z)
        oob(-    1,      y,      z)
        oob(-   10,      y,      z)
      }
      for {
        x ← 0 until m
        z ← 0 until o
      } {
        oob(     x,  n + 1,      z)
        oob(     x,      n,      z)
        oob(     x, -    1,      z)
        oob(     x, -   10,      z)
      }
      for {
        x ← 0 until m
        y ← 0 until n
      } {
        oob(     x,      y,  o + 1)
        oob(     x,      y,      o)
        oob(     x,      y, -    1)
        oob(     x,      y, -   10)
      }
    }
  }

  test("convenience constructor") {
    Vector.conv(
      Seq("0,0", "0,1"),
      Seq("1,0", "1,1"),
      Seq("2,0", "2,1")
    )

    Vector.conv(
      Seq(
        Seq("0,0,0", "0,0,1"),
        Seq("0,1,0", "0,1,1"),
        Seq("0,2,0", "0,2,1")
      ),
      Seq(
        Seq("1,0,0", "1,0,1"),
        Seq("1,1,0", "1,1,1"),
        Seq("1,2,0", "1,2,1")
      )
    )

    Vector.conv(
      Seq(
        Seq(
          Seq("0,0,0,0", "0,0,0,1"),
          Seq("0,0,1,0", "0,0,1,1"),
          Seq("0,0,2,0", "0,0,2,1")
        ),
        Seq(
          Seq("0,1,0,0", "0,1,0,1"),
          Seq("0,1,1,0", "0,1,1,1"),
          Seq("0,1,2,0", "0,1,2,1")
        )
      ),
      Seq(
        Seq(
          Seq("1,0,0,0", "1,0,0,1"),
          Seq("1,0,1,0", "1,0,1,1"),
          Seq("1,0,2,0", "1,0,2,1")
        ),
        Seq(
          Seq("1,1,0,0", "1,1,0,1"),
          Seq("1,1,1,0", "1,1,1,1"),
          Seq("1,1,2,0", "1,1,2,1")
        )
      )
    )

    //    Vector(
//      Vector(2 :: ⊥, "0,0", "0,1"),
//      Vector(2 :: ⊥, "1,0", "1,1"),
//      Vector(2 :: ⊥, "2,0", "2,1")
//    )
  }
}
