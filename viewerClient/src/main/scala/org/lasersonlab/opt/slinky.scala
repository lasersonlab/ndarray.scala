package org.lasersonlab.opt

import hammerlab.option.{ Non, Som }
import org.lasersonlab.uri.?
import slinky.readwrite.{ Reader, Writer }

import scala.scalajs.js

trait slinky {
  implicit def optReader[T](implicit r: Reader[T]): Reader[?[T]] =
    new Reader[?[T]] {
      override protected def forceRead(o: js.Object): ?[T] = if (o == null) Non else Some(r.read(o))
    }
  implicit def optWriter[T](implicit w: Writer[T]): Writer[?[T]] =
    new Writer[?[T]] {
      override def write(p: ?[T]): js.Object =
        p match {
          case Non ⇒ null
          case Som(t) ⇒ w.write(t)
        }
    }


}
