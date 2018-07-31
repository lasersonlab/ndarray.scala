package org.lasersonlab.ndarray

import hammerlab.shapeless._
import org.lasersonlab.ndarray.TList.{ Base, Tail }
import shapeless.Nat._

class TListTest
  extends hammerlab.Suite {

  test("one") {
    val one = TList(10)
    implicitly[=:=[one.Size, _1]]
    ==(one.size, 1)
    ==(one.head, 10)
    ==(one.tail, None)
  }
  test("two") {
    val two = TList(10, 20)
    implicitly[=:=[two.Size, _2]]
    ==(two.size, 2)
    ==(two.head, 10)
    two match {
      case Tail(10, Base(20)) ⇒
      case _ ⇒ fail()
    }

    two match {
      case Tail(10, one) ⇒
        ==(one.head, 20)
        one.tail should be(None)
      case _ ⇒ fail()
    }

    ==(two._rest.head, 20)
  }

  test("three") {
    val three = TList(10, 20, 30)

    implicitly[=:=[three.Size, _3]]
    ==(three.size, 3)
    ==(three.head, 10)
    three match {
      case Tail(10, Tail(20, Base(30))) ⇒
      case _ ⇒ fail()
    }

    three match {
      case Tail(10, two) ⇒
        ==(two.head, 20)
        two.tail should be(Some(Base(30)))
        two.tail should be(Some(TList(30)))
      case _ ⇒ fail()
    }

    ==(three._rest.head, 20)
    three.tail should be(Some(Tail(20, Base(30))))
    three.tail should be(Some(TList(20, 30)))
  }
}
