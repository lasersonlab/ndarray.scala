package org.lasersonlab.uri

import cats.implicits._

import utest._

object LocalTest
  extends TestSuite {

  import scala.concurrent.ExecutionContext.Implicits.global

  val tests = Tests{
    'local - {
      val shared = "shared/src/test/resources/test.txt"
      val uri = s"uri/$shared"
      val resource =
        if (Local(uri).existsSync)
          Local(uri)
        else
          Local(s"../$shared")

      resource
        .string
        .map {
          actual ⇒
          assert(
            actual ==
              """a
                |12
                |abc
                |1234
                |abcde
                |"""
                .stripMargin

          )
        }
    }
  }
}
