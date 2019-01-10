package org.lasersonlab.uri

import java.net.URI

import cats.implicits._
import org.lasersonlab.uri.http.NodeHttp
import utest._

import scala.concurrent.ExecutionContext.Implicits.global

object NodeHttpTest
  extends TestSuite {
  val tests = Tests {
    'txt - {
      val url = "https://storage.googleapis.com/runsascoded/test.txt"
      NodeHttp(
        new URI(url)
      )
      .string
      .map {
        body ⇒
          assert(body == "123\n456\n")
      }
    }

    'png - {
      val url = "https://storage.googleapis.com/runsascoded/test.png"
      NodeHttp(
        new URI(url)
      )
      .read
      .map {
        body ⇒
          assert(body.length == 850)
      }
    }
  }
}
