package org.lasersonlab.shapeless

import cats.implicits._

class SListTest
  extends hammerlab.Suite
    with SList.instances {
  import SList._

  val s0 =                                           ⊥
  val s1 = 111                                    :: ⊥
  val s2 = 111 :: 222                             :: ⊥
  val s3 = 111 :: 222 :: 333                      :: ⊥
  val s4 = 111 :: 222 :: 333 :: 444               :: ⊥
  val s5 = 111 :: 222 :: 333 :: 444 :: 555        :: ⊥
  val s6 = 111 :: 222 :: 333 :: 444 :: 555 :: 666 :: ⊥

  test("reduce") {
    import cats.instances.int.catsKernelStdGroupForInt
    ==( s1 reduce,  111 )
    ==( s2 reduce,  333 )
    ==( s3 reduce,  666 )
    ==( s4 reduce, 1110 )
    ==( s5 reduce, 1665 )
    ==( s6 reduce, 2331 )
  }

  test("foldLeft") {
    ==( ( s0 : `0`[Int] ).foldLeft (0)(_ + _),    0 )
    ==( ( s1 : `1`[Int] ).foldLeft (0)(_ + _),  111 )
    ==( ( s2 : `2`[Int] ).foldLeft (0)(_ + _),  333 )
    ==( ( s3 : `3`[Int] ).foldLeft (0)(_ + _),  666 )
    ==( ( s4 : `4`[Int] ).foldLeft (0)(_ + _), 1110 )
    ==( ( s5 : `5`[Int] ).foldLeft (0)(_ + _), 1665 )
    ==( ( s6 : `6`[Int] ).foldLeft (0)(_ + _), 2331 )

    ==(   s1             .foldLeft (0)(_ + _),  111 )
    ==(   s2             .foldLeft (0)(_ + _),  333 )
    ==(   s3             .foldLeft (0)(_ + _),  666 )
    ==(   s4             .foldLeft (0)(_ + _), 1110 )
    ==(   s5             .foldLeft (0)(_ + _), 1665 )
    ==(   s6             .foldLeft (0)(_ + _), 2331 )
  }

  test("foldRight") {
    import cats.Eval
    import Eval.always
    implicit def wrap[T](t: T): Eval[T] = always(t)
    ==( ( s0 : `0`[Int] ).foldRight(0)((a, lb) ⇒ lb.map(a + _)),    0 )
    ==( ( s1 : `1`[Int] ).foldRight(0)((a, lb) ⇒ lb.map(a + _)),  111 )
    ==( ( s2 : `2`[Int] ).foldRight(0)((a, lb) ⇒ lb.map(a + _)),  333 )
    ==( ( s3 : `3`[Int] ).foldRight(0)((a, lb) ⇒ lb.map(a + _)),  666 )
    ==( ( s4 : `4`[Int] ).foldRight(0)((a, lb) ⇒ lb.map(a + _)), 1110 )
    ==( ( s5 : `5`[Int] ).foldRight(0)((a, lb) ⇒ lb.map(a + _)), 1665 )
    ==( ( s6 : `6`[Int] ).foldRight(0)((a, lb) ⇒ lb.map(a + _)), 2331 )

    ==(   s1             .foldRight(0)((a, lb) ⇒ lb.map(a + _)),  111 )
    ==(   s2             .foldRight(0)((a, lb) ⇒ lb.map(a + _)),  333 )
    ==(   s3             .foldRight(0)((a, lb) ⇒ lb.map(a + _)),  666 )
    ==(   s4             .foldRight(0)((a, lb) ⇒ lb.map(a + _)), 1110 )
    ==(   s5             .foldRight(0)((a, lb) ⇒ lb.map(a + _)), 1665 )
    ==(   s6             .foldRight(0)((a, lb) ⇒ lb.map(a + _)), 2331 )
  }

  test("size") {
    ==(s0.size, 0)
  }
}
