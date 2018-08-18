package org.lasersonlab

import java.io.FileNotFoundException

import hammerlab.path._
import hammerlab.str._
import org.lasersonlab.ndarray.Arithmetic

package object zarr
  extends TListDecoders
     with OptDecoder
     with Arithmetic.HasOps {

  // TODO: move these to appropriate dependencies

  type |[+L, +R] = Either[L, R]

  object Int {
    def unapply(s: List[Char]): Option[Int] = Some( s.mkString.toInt )
  }

  implicit class PathOps(val dir: Path) extends AnyVal {
    def ?(basename: Str): FileNotFoundException | Path = {
      val path = dir / basename
      if (!path.exists)
        Left(
          new FileNotFoundException(
            path.toString
          )
        )
      else
        Right(path)
    }
  }
}
