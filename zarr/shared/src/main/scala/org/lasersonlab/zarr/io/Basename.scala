package org.lasersonlab.zarr.io

import hammerlab.str._
import org.lasersonlab.uri.Uri.Segment

case class Basename[-T](override val toString: String)

object Basename {
  implicit def toString[T](basename: Basename[T]): String = basename.toString
  implicit def toStr   [T](basename: Basename[T]): Str    = basename.toString

  implicit def segment[T]: Segment[Basename[T]] = { _.toString }
}
