package org.lasersonlab.netcdf

import hammerlab.lines._
import hammerlab.show._
import ucar.ma2.DataType
import ucar.nc2.{ Attribute, NetcdfFile }

trait show {
  implicit val showDataType: Show[DataType] = Show { _.toString }

  implicit def attributeLines: ToLines[Attribute] =
    ToLines {
      attr ⇒
        attr.getLength match {
          case 1 ⇒
            s"${attr.getShortName}: ${attr.getValue(0)}"
          case n ⇒
            Lines(
              s"${attr.getShortName} (${attr.getLength}):",
              indent(
                if (n < 10)
                  (0 until n).map(attr.getValue(_).toString)
                else
                  Lines(
                    (0 until 10).map(attr.getValue(_).toString),
                    "…"
                  )
              )
            )
        }
    }

  def label[T: ToLines](name: String, values: Seq[T]): Lines =
    if (values.nonEmpty)
      Lines(
        s"$name:",
        indent(values)
      )
    else
      Lines()

  implicit def attributesLines: ToLines[Seq[Attribute]] =
    ToLines { label("attrs", _) }

  implicit def showDimensions: Show[Seq[Dimension]] =
    Show {
      _
        .map(_.show)
        .mkString("ｘ")
    }

  implicit def showFile: ToLines[NetcdfFile] =
    ToLines {
      file ⇒
        Lines(
          s"${file.getLocation}:",
          indent(file.getRootGroup: Group)
        )
    }
}

object show extends show
