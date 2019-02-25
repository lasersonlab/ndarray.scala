package org.lasersonlab.uri

import java.net.URI

import cats.implicits._
import utest._

import scala.concurrent.ExecutionContext.Implicits.global

object HttpTest
  extends TestSuite
     with http.Test {
  val tests = Tests {
    'txt - {
      val url = "https://storage.googleapis.com/runsascoded/test.txt"
      Http(new URI(url))
        .string
        .map {
          body ⇒
            assert(body == "123\n456\n")
        }
    }

    'png - {
      val url = "https://storage.googleapis.com/runsascoded/test.png"
      Http(new URI(url))
        .read
        .map {
          body ⇒
            assert(body.length == 850)
        }
    }
  }
}
