package org.lasersonlab.shapeless

import cats.implicits._

class SListTest
  extends hammerlab.Suite
    with SList.instances {
  import SList._
  test("value apply") {
    Ops[Int, `0`](`0`).::(111)

    import cats.instances.int.catsKernelStdGroupForInt
    ==( 111                                    :: ⊥ reduce,  111 )
    ==( 111 :: 222                             :: ⊥ reduce,  333 )
    ==( 111 :: 222 :: 333                      :: ⊥ reduce,  666 )
    ==( 111 :: 222 :: 333 :: 444               :: ⊥ reduce, 1110 )
    ==( 111 :: 222 :: 333 :: 444 :: 555        :: ⊥ reduce, 1665 )
    ==( 111 :: 222 :: 333 :: 444 :: 555 :: 666 :: ⊥ reduce, 2331 )

    ==( (111                                    :: ⊥ : `1`[Int]).foldLeft(0)(_ + _),  111 )
    ==( (111 :: 222                             :: ⊥ : `2`[Int]).foldLeft(0)(_ + _),  333 )
    ==( (111 :: 222 :: 333                      :: ⊥ : `3`[Int]).foldLeft(0)(_ + _),  666 )
    ==( (111 :: 222 :: 333 :: 444               :: ⊥ : `4`[Int]).foldLeft(0)(_ + _), 1110 )
    ==( (111 :: 222 :: 333 :: 444 :: 555        :: ⊥ : `5`[Int]).foldLeft(0)(_ + _), 1665 )
    ==( (111 :: 222 :: 333 :: 444 :: 555 :: 666 :: ⊥ : `6`[Int]).foldLeft(0)(_ + _), 2331 )

    ==( (111                                    :: ⊥           ).foldLeft(0)(_ + _),  111 )
    ==( (111 :: 222                             :: ⊥           ).foldLeft(0)(_ + _),  333 )
    ==( (111 :: 222 :: 333                      :: ⊥           ).foldLeft(0)(_ + _),  666 )
    ==( (111 :: 222 :: 333 :: 444               :: ⊥           ).foldLeft(0)(_ + _), 1110 )
    ==( (111 :: 222 :: 333 :: 444 :: 555        :: ⊥           ).foldLeft(0)(_ + _), 1665 )
    ==( (111 :: 222 :: 333 :: 444 :: 555 :: 666 :: ⊥           ).foldLeft(0)(_ + _), 2331 )
  }
}
