package org.lasersonlab.zarr.cmp

import cats.implicits._

trait path
  extends Cmp.ops {
  import hammerlab.path._
  implicit val path: Cmp[Path] =
    Cmp {
      (l, r) ⇒
        (l.isDirectory, r.isDirectory) match {
          case ( true,  true) ⇒ cmp(l.list.toList.sortBy(_.toString), r.list.toList.sortBy(_.toString))
          case (false, false) ⇒ cmp(l.read              , r.read              )
          case ( true, false) ⇒ Some(s"$l is a directory, $r is a file"       )
          case (false,  true) ⇒ Some(s"$l is a file, $r is a directory"       )
        }
    }
}
