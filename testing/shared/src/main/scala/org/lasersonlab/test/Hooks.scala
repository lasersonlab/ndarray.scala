package org.lasersonlab.test

import java.util.concurrent.Executor

import cats.implicits._
import lasersonlab.future.F
import utest.TestSuite

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import ExecutionContext.fromExecutor

trait HasExecutionContext {
  implicit def ec: ExecutionContext
  object threads {
    implicit val single =
      fromExecutor(
        (task: Runnable) => task.run()
      )
  }
}

trait Befores {
  self: TestSuite ⇒
  protected val befores = ArrayBuffer[() ⇒ F[Unit]]()
  def before(fn: ⇒ F[Unit]): Unit = befores += (() ⇒ fn)
}

trait Afters {
  self: TestSuite ⇒
  protected val afters = ArrayBuffer[() ⇒ F[Unit]]()
  def after(fn: ⇒ F[Unit]): Unit = afters += (() ⇒ fn)
}

trait BeforeAlls
  extends TestSuite
     with Befores
     with HasExecutionContext
{
  protected val beforeAlls = ArrayBuffer[() ⇒ F[Unit]]()
  def beforeAll(fn: ⇒ F[Unit]): Unit = beforeAlls += (() ⇒ fn)
  lazy val beforeAllResult = beforeAlls.map(_()).toList.sequence.map { _ ⇒ () }
}

trait AfterAlls
  extends TestSuite
     with Afters
     with HasExecutionContext
{
  protected val afterAlls = ArrayBuffer[() ⇒ Unit]()
  def afterAll(fn: ⇒ Unit): Unit = afterAlls += (() ⇒ fn)
  final override def utestAfterAll(): Unit = {
    super.utestAfterAll()
    afterAlls.foreach(_())
  }
}

trait Hooks
  extends TestSuite
     with FuturizeHook
     with Befores
     with Afters
     with BeforeAlls
     with AfterAlls
{
self: TestSuite ⇒
  override def utestWrap(
    path: Seq[String],
    runBody: ⇒ concurrent.Future[Any]
  )(
    implicit
    ec: ExecutionContext
  ):
    concurrent.Future[Any] =
  {
    for {
      allBefores ←
        (
          beforeAllResult ::
          befores
            .map(_())
            .toList
        )
        .sequence
      result ← runBody
      allAfters ←
        afters
          .map(_())
          .toList
        .sequence
    } yield
      result
  }
}

trait FuturizeHook {
  implicit def _futurizeHook(fn: () ⇒ Unit)(implicit ec: ExecutionContext): () ⇒ F[Unit] = () ⇒ F { fn() }
}
