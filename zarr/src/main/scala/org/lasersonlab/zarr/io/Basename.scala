package org.lasersonlab.zarr.io

import hammerlab.str._

case class Basename[-T](override val toString: String)

object Basename {
  implicit def toString[T](basename: Basename[T]): String = basename.toString
  implicit def toStr   [T](basename: Basename[T]): Str    = basename.toString
}
