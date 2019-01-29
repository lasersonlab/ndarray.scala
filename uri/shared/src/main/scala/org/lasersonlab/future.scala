package org.lasersonlab

import java.lang.System.err

import lasersonlab.future.F

import scala.concurrent.ExecutionContext
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
    def logError(implicit ec: ExecutionContext): F[T] = onError(err.println(_))
  }
  trait syntax {
    @inline implicit def makeFutureOps[T](f: F[T]): Ops[T] = Ops(f)
  }
}
