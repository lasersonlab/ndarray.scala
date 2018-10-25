package org.hammerlab.paths

import java.io.FileNotFoundException

import hammerlab.either._
import hammerlab.path.Path
import hammerlab.str._

trait HasPathOps {
  implicit val makePathSymbolOps = HasPathOps.PathSymbolOps _
  implicit val makePathStringOps = HasPathOps.PathStringOps _
  implicit val makePathOps = HasPathOps.PathOps _
}

object HasPathOps {
  implicit class PathSymbolOps(val s: Symbol) extends AnyVal {
    def / (t: Str): Path = Path(s.name) / t
  }
  implicit class PathStringOps(val s: String) extends AnyVal {
    def / (t: Str): Path = Path(s) / t
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
