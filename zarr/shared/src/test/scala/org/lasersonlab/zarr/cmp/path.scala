package org.lasersonlab.zarr.cmp

import cats.implicits._
import lasersonlab.zarr.Path
import org.lasersonlab.test.future.{ CanEq, Cmp }

import scala.concurrent.ExecutionContext

trait path
  extends CanEq.syntax {
  implicit def path(implicit ec: ExecutionContext): Cmp[Path] = ???
//    {
//      (l, r) ⇒
//        (l.isDirectory, r.isDirectory) match {
//          case ( true,  true) ⇒ cmp(l.list.map(_.sortBy(_.toString)), r.list.map(_.sortBy(_.toString)))
//          case (false, false) ⇒ cmp(l.read                   , r.read              )
//          case ( true, false) ⇒ Future(Some((s"$l is a directory, $r is a file")))
//          case (false,  true) ⇒ Future(Some((s"$l is a file, $r is a directory")))
//        }
//    }
}
