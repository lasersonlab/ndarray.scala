package org.lasersonlab

import caseapp.core.Error.MalformedValue
import caseapp.core.argparser._

import scala.concurrent.duration._

  trait duration
extends duration.syntax {

  // Help this mix-in replace `import scala.concurrent.duration._` with e.g. `import lasersonlab.duration._`, which
  // provides other functionality as well
  type Duration = scala.concurrent.duration.Duration

  object Hours {
    val regex = """(\d+) ?(?:h|hrs?|hours?)""".r
    def unapply(s: String): Option[Int] =
      s match {
        case regex(s) ⇒ Some(s.toInt)
        case _ ⇒ None
      }
  }
  object Minutes {
    val regex = """(\d+) ?(?:m|mins?|minutes?)""".r
    def unapply(s: String): Option[Int] =
      s match {
        case regex(s) ⇒ Some(s.toInt)
        case _ ⇒ None
      }
  }
  object Seconds {
    val regex = """(\d+) ?(?:s|seconds?)""".r
    def unapply(s: String): Option[Int] =
      s match {
        case regex(s) ⇒ Some(s.toInt)
        case _ ⇒ None
      }
  }
  object Millis {
    val regex = """(\d+) ?(?:ms)""".r
    def unapply(s: String): Option[Long] =
      s match {
        case regex(s) ⇒ Some(s.toLong)
        case _ ⇒ None
      }
  }

  implicit val parseDuration: ArgParser[Duration] =
    SimpleArgParser(
      "duration",
      {
        case   Hours(n) ⇒ Right( n   hours )
        case Minutes(n) ⇒ Right( n minutes )
        case Seconds(n) ⇒ Right( n seconds )
        case  Millis(n) ⇒ Right( n  millis )
        case s ⇒ Left(MalformedValue("duration", s))
      }
    )
}
object duration {
  trait syntax {
    @inline implicit def  IntDurationOps(n:  Int) = DurationInt (n)
    @inline implicit def LongDurationOps(n: Long) = DurationLong(n)
  }
}
