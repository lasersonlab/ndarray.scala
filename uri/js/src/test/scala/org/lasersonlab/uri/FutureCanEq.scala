package org.lasersonlab.uri

//import org.hammerlab.cmp.CanEq
//import org.scalatest.FunSuite

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.util.{ Failure, Success }

//trait FutureCanEq {
//  self: hammerlab.Suite ⇒
//  implicit val timeout: Duration = 10 seconds
//  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
//  implicit def futureCanEq[L, R](implicit c: CanEq[L, R]): CanEq.Aux[Future[L], R, c.Diff] =
//    new CanEq[Future[L], R] {
//      type Diff = c.Diff
//      def cmp(l: Future[L], r: R): Option[Diff] =
//        c(
//          Await.result(l, timeout),
//          r
//        )
//    }

//  import scala.scalajs.js.timers._
//  def futureEq[L, R](l: Future[L], r: R)(implicit c: CanEq[L, R]) = {
//    val handle =
//      setTimeout(1000) {
//        l
//          .value
//          .fold {
//            fail(s"Future timed out: $l")
//          } {
//            case Failure(e) ⇒ fail(s"Future failed: $e")
//            case Success(l) ⇒ c(l, r).foreach { diff ⇒ fail(s"Expected: $r, actual: $l: $diff") }
//          }
//      }
//
//    l.foreach { _ ⇒ clearTimeout(handle) }
//  }
//}
