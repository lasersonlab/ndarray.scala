package org.lasersonlab.circe

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.lasersonlab.circe.JsonSingletonTest._
import shapeless._
import SingletonCodec._

class JsonSingletonTest
  extends hammerlab.Suite {
  test("foo") {
    new Foo("abc").asJson should be(Json.fromString("abc"))
    Foo1("abc").asJson should be(Json.fromString("abc"))
    Foo2(123).asJson should be(Json.fromInt(123))
  }
}

object JsonSingletonTest {
  class Foo(override val toString: String) extends AnyVal
  case class Foo1(override val toString: String)
  case class Foo2(n: Int)
}
