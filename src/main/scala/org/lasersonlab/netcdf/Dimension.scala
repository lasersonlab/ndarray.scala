package org.lasersonlab.netcdf

import hammerlab.show._

case class Dimension(name: String, size: Int)
object Dimension {
  implicit def apply(d: ucar.nc2.Dimension): Dimension =
    Dimension(d.getShortName, d.getLength)

  implicit def show: Show[Dimension] =
    Show {
      case Dimension(name, size) ⇒
        Option(name) match {
          case Some(name) ⇒ show"($name: $size)"
          case _ ⇒ show"$size"
        }

    }
}
