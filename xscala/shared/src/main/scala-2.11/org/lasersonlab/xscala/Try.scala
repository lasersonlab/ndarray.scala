package org.lasersonlab.xscala

object Try {
  import scala.util.{ Failure, Success, Try }
  implicit class Ops[T](val t: Try[T]) extends AnyVal {
    def toEither: Either[Throwable, T] =
      t match {
        case Failure(e) ⇒  Left(e)
        case Success(t) ⇒ Right(t)
      }

    def fold[O](l: Throwable ⇒ O, r: T ⇒ O): O =
      t match {
        case Failure(t) ⇒ l(t)
        case Success(t) ⇒ r(t)
      }
  }
}
