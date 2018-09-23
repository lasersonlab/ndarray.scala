package org.lasersonlab.shapeless

import cats.implicits._

class TListTest
  extends hammerlab.Suite
     with TList.instances {
  import TList._
  test("type apply") {
    !![`0`.type =:= `0`[Int]]
    type I = Int
    !![ (                          I :: ⊥ ) =:= `1`[I] ]
    !![ (                     I :: I :: ⊥ ) =:= `2`[I] ]
    !![ (                I :: I :: I :: ⊥ ) =:= `3`[I] ]
    !![ (           I :: I :: I :: I :: ⊥ ) =:= `4`[I] ]
    !![ (      I :: I :: I :: I :: I :: ⊥ ) =:= `5`[I] ]
    !![ ( I :: I :: I :: I :: I :: I :: ⊥ ) =:= `6`[I] ]
  }

  test("value apply") {
    import cats.instances.int.catsKernelStdGroupForInt
    (111 :: ⊥ reduce) should be(111)
    (111                                    :: ⊥ : `1`[Int]).foldLeft(0)(_ + _) should be(111)
    (111 :: 222                             :: ⊥ : `2`[Int]).foldLeft(0)(_ + _) should be(333)
    (111 :: 222 :: 333                      :: ⊥ : `3`[Int]).foldLeft(0)(_ + _) should be(666)
    (111 :: 222 :: 333 :: 444               :: ⊥ : `4`[Int]).foldLeft(0)(_ + _) should be(1110)
    (111 :: 222 :: 333 :: 444 :: 555        :: ⊥ : `5`[Int]).foldLeft(0)(_ + _) should be(1665)
    (111 :: 222 :: 333 :: 444 :: 555 :: 666 :: ⊥ : `6`[Int]).foldLeft(0)(_ + _) should be(2331)
  }
}
