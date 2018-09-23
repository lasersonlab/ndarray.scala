package org.lasersonlab.shapeless

class TListTest
  extends hammerlab.Suite
     with TList.instances {
  import TList._
  test("apply") {
    !![`0`.type =:= `0`[Int]]
    !![(                                   Int :: `0`) =:= `1`[Int]]
    !![(                            Int :: Int :: `0`) =:= `2`[Int]]
//    !![(                     Int :: Int :: Int :: `0`) =:= `3`[Int]]
//    !![(              Int :: Int :: Int :: Int :: `0`) =:= `4`[Int]]
//    !![(       Int :: Int :: Int :: Int :: Int :: `0`) =:= `5`[Int]]
//    !![(Int :: Int :: Int :: Int :: Int :: Int :: `0`) =:= `6`[Int]]
  }
}
