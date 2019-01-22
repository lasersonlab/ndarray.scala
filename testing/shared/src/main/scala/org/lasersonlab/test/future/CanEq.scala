package org.lasersonlab.test.future

import lasersonlab.future.F
import org.hammerlab.cmp

import scala.concurrent.{ ExecutionContext, Future }

trait CanEq[L, R] {
  type Diff
  type Result = F[Option[Diff]]
  def apply(l: L, r: R): F[Option[Diff]]
}
trait CanEqTop {
  type Aux[L, R, D] = CanEq[L, R] { type Diff = D }
}
trait CanEqFromHammerLab
extends CanEqTop
{
  implicit def fromHammerLab[L, R](
    implicit
    ce: cmp.CanEq[L, R],
    ec: ExecutionContext
  ):
    Aux[L, R, ce.Diff] =
    new CanEq[L, R] {
      type Diff = ce.Diff
      def apply(l: L, r: R): Result = F { ce(l, r) }
    }
}
trait CanEqFromLasersonLab
extends CanEqFromHammerLab
{
  implicit def fromLasersonLab[T](
    implicit
    ce: org.lasersonlab.test.Cmp[T],
    ec: ExecutionContext
  ):
    Cmp.Aux[T, ce.Diff] =
    new CanEq[T, T] {
      type Diff = ce.Diff
      def apply(l: T, r: T): Result = F { ce(l, r) }
    }
}
trait CanEqFuturizeLeft
extends CanEqFromLasersonLab
{
  implicit def futurizeLeft[L, R](
    implicit
    ce: CanEq[L, R],
    ec: ExecutionContext
  ):
    Aux[F[L], R, ce.Diff] =
    new CanEq[F[L], R] {
      type Diff = ce.Diff
      def apply(l: F[L], r: R): Result =
        for {
          l ← l
          res ← ce(l, r)
        } yield
          res
    }
}
object CanEq
extends CanEqFuturizeLeft
{
  implicit def futurizeBoth[L, R](
    implicit
    ce: CanEq[L, R],
    ec: ExecutionContext
  ):
    Aux[F[L], F[R], ce.Diff] =
    new CanEq[F[L], F[R]] {
      type Diff = ce.Diff
      def apply(l: F[L], r: F[R]): Result =
        for {
          l ← l
          r ← r
          res ← ce(l, r)
        } yield
          res
    }

  trait syntax {
    def cmp[L, R](l: L, r: R)(implicit c: CanEq[L, R]): Future[Option[c.Diff]] = c(l, r)
  }
}

trait Assert[L, R] {
  def apply(l: L, r: R): F[Unit]
}
object Assert
{
  case class ComparisonFailure[D](diff: D) extends RuntimeException

  def f[L, R](l: L, r: R)(implicit c: CanEq[L, R], ec: ExecutionContext): F[Unit] =
    c(l, r).map {
      case Some(diff) ⇒ throw ComparisonFailure(diff)
      case None       ⇒ ()
    }

  implicit def futureizeNeither[L, R](implicit c: CanEq[L, R], ec: ExecutionContext): Assert[L, R] = f(_, _)

  trait syntax {
    def ==[L, R](l: L, r: R)(implicit a: Assert[L, R]): F[Unit] = a(l, r)
    def !![Diff](diff: Diff): F[Unit] = Future.failed(ComparisonFailure(diff))
  }
}
