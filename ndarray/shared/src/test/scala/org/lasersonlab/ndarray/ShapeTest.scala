package org.lasersonlab.ndarray

import hammerlab.shapeless._
import org.lasersonlab.ndarray.Shape.{ Base, Tail }
import shapeless.Nat._

class ShapeTest
  extends hammerlab.Suite {

  test("one") {
    val one = Shape(10)
    implicitly[=:=[one.Size, _1]]
    ==(one.size, 1)
    ==(one.n, 10)
    ==(one.rest, None)
  }
  test("two") {
    val two = Shape(10, 20)
    implicitly[=:=[two.Size, _2]]
    ==(two.size, 2)
    ==(two.n, 10)
    two match {
      case Tail(10, Base(20)) ⇒
      case _ ⇒ fail()
    }

    two match {
      case Tail(10, one) ⇒
        ==(one.n, 20)
        one.rest should be(None)
      case _ ⇒ fail()
    }

    ==(two._rest.n, 20)
  }

  test("three") {
    val three = Shape(10, 20, 30)

    implicitly[=:=[three.Size, _3]]
    ==(three.size, 3)
    ==(three.n, 10)
    three match {
      case Tail(10, Tail(20, Base(30))) ⇒
      case _ ⇒ fail()
    }

    three match {
      case Tail(10, two) ⇒
        ==(two.n, 20)
        two.rest should be(Some(Base(30)))
        two.rest should be(Some(Shape(30)))
      case _ ⇒ fail()
    }

    ==(three._rest.n, 20)
    three.rest should be(Some(Tail(20, Base(30))))
    three.rest should be(Some(Shape(20, 30)))
  }
}
