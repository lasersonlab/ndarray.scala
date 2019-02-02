package org.lasersonlab

import java.lang.System.err

import lasersonlab.future.F

import scala.concurrent.{ Await, ExecutionContext }
import scala.concurrent.duration.Duration
import scala.util.Failure

object future {
  implicit class Ops[T](val f: F[T]) extends AnyVal {
    def onError(fn: Throwable ⇒ Unit)(implicit ec: ExecutionContext): F[T] = {
      f
        .onComplete {
          case Failure(e) ⇒ fn(e)
          case _ ⇒
        }
      f
    }
    def   logError(implicit ec: ExecutionContext): F[T] = onError { err.println(_) }
    def throwError(implicit ec: ExecutionContext): F[T] = onError { throw _ }
    def await(implicit duration: Duration): T = Await.result(f, duration)
  }
  trait syntax {
    @inline implicit def makeFutureOps[T](f: F[T]): Ops[T] = Ops(f)
  }
}
