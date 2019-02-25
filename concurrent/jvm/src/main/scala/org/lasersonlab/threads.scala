package org.lasersonlab

import cats.implicits._

import java.util.concurrent.{ LinkedBlockingQueue, ThreadPoolExecutor }
import java.util.concurrent.TimeUnit.SECONDS

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor
import scala.concurrent.duration._
import Runtime.getRuntime

import caseapp.core.Error.MalformedValue
import caseapp.core.argparser.{ ArgParser, SimpleArgParser }

import scala.util.Try

trait threads {
  object pool {
    implicit val `1` = fromExecutor((task: Runnable) ⇒ task.run())
    implicit val `2` = apply(2)
    implicit val global = ExecutionContext.Implicits.global
    implicit val default = global
  }

  case class NumThreads(value: Int)
  object NumThreads {
    implicit val default = NumThreads(cores)
    implicit def   wrapNumThreads(value: Int): NumThreads = NumThreads(value)
    implicit def unwrapNumThreads(numThreads: NumThreads): Int = numThreads.value
    implicit val argParser: ArgParser[NumThreads] =
      SimpleArgParser(
        "num threads",
        {
          s ⇒
            Try {
              NumThreads(
                s match {
                  case s if s.charAt(0) == 'x' ⇒ cores * s.drop(1).toInt
                  case s ⇒ s.toInt
                }
              )
            }
            .toEither
            .leftMap {
              _ ⇒ MalformedValue("num threads", s)
            }
        }
      )
  }

  def cores = getRuntime.availableProcessors

  def apply(
    numThreads: NumThreads = cores,
    duration: Duration = 1 second
  ) =
    fromExecutor(
      new ThreadPoolExecutor(
        numThreads.value, numThreads.value,
        1, SECONDS,
        new LinkedBlockingQueue()
      )
    )
}
