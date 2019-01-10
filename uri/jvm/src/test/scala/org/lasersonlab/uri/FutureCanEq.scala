package org.lasersonlab.uri

import org.hammerlab.cmp.CanEq

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

trait FutureCanEq {
  implicit val timeout: Duration = 10 seconds
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
  implicit def futureCanEq[L, R](implicit c: CanEq[L, R]): CanEq.Aux[Future[L], R, c.Diff] =
    new CanEq[Future[L], R] {
      type Diff = c.Diff
      def cmp(l: Future[L], r: R): Option[Diff] =
        c(
          Await.result(l, timeout),
          r
        )
    }
}
