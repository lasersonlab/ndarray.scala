package org.lasersonlab.uri

import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global

class HttpTest
  extends hammerlab.Suite {
//  implicit lazy val context: ContextShift[IO] = IOContextShift.global
//  test("google") {
//    Http[IO](new URI("https://google.com")).string.unsafeRunAsync {
//      case Right(body) ⇒
//        println(s"body: $body")
//    }

//    var sum = 0
//    for { i ← 0 until 100000000 } {
//      if (i % 10000000 == 0)
//        println(i)
//
//        sum += i * i * i
//        sum = -sum
//    }

    //Thread.sleep(1000)

//    println("exiting…")
//    Http[IO](new URI("https://google.com")).string.unsafeRunAsync {
//      case Right(body) ⇒
//        println("checking")
//        ==(
//          body,
//          ""
//        )
//    }
//  }
}

/*

object Test {
  import org.lasersonlab.uri._

  import java.net.URI

  import cats.effect.IO
  import cats.implicits._

  import scala.concurrent.ExecutionContext.Implicits.global

  Http[IO](new URI("https://google.com")).string.unsafeRunAsync {
    case Right(body) ⇒
      println(s"body: $body")
  }
}
*/
