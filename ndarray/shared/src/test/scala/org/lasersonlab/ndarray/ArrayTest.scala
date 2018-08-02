package org.lasersonlab.ndarray

//import org.lasersonlab.shapeless.TList.Base
import shapeless._
import nat._
import hammerlab.shapeless._
import org.lasersonlab.ndarray.Array.Idx
import org.lasersonlab.shapeless.TList

class ArrayTest
  extends hammerlab.Suite {
//  test("1-D") {
//    val a = Array.Base(Seq(1, 2, 3, 4))
//    ==(a(0), 1)
//    ==(a(1), 2)
//    ==(a(2), 3)
//    ==(a(3), 4)
//    ==(a.shape, 4)
//  }
//
//  test("2-D") {
//
//    val row: Array.Base[Int] = 10 to 20
//
//
//    implicit def `1D`[T](s: Seq[T]): Array.Of[Int, _1, Idx[_1]] = ???
//    implicit def `2D`[T](s: Seq[Seq[T]]): Array.Of[Int, _2, Idx[_2]] = ???
//
//    (10 to 20): Array[Int]
//    (10 to 20): Array.Of[Int, _1, Idx[_1]]
//
//    Seq(10 to 20, 30 to 40): Array[Int]
//    Seq(10 to 20, 30 to 40): Array.Of[Int, _1, TList.Of[Int, _1]]
//
//    Array(10 to 20: Array.Of[Int, _1, Idx[_1]])
//
////    Array[Int, _1, Idx[_1], Array[Int]](10 to 20)
//
//    Array[Int, _1, Idx[_1], Array.Of[Int, _1, Idx[_1]]](10 to 20)
//    Array[Int, _1, Idx[_1], Array.Of[Int, _1, Idx[_1]]](10 to 20, 30 to 40)
//
//    Array[Int, _1, TList.Base[Int], Array.Base[Int]](10 to 20)
//    Array[Int, _1, TList.Base[Int], Array.Base[Int]](10 to 20, 30 to 40)
//
//    Array[
//      Int,
//      _2,
//      Idx[_2],
//      Array.Of[Int, _2, Idx[_2]]
//    ](
//      Seq(10 to 20, 30 to 40),
//      Seq(50 to 60, 70 to 80)
//    )
//
////    Array(10 to 20, 30 to 40)
//
////    (10 to 20): Array.Base[Int]
////    (10 to 20): Array.Of[Int, _1, TList.Base[Int]]
//
////    implicit val pshape: Idx[_1] = ???
////    val row2: Array[Int] = Array.lift(Seq(10 to 20: Array.Base[Int]))
//
////    val row3: Array[Int] = Array.lift(Seq(10 to 20))
//
////    val a: Array[Int] =
////      Array.lift(
////        Seq(
////          10 to 20,
////          30 to 40,
////          50 to 60
////        )
////      )
//  }
}
