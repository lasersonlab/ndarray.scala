package org.lasersonlab.netcdf

import hammerlab.lines._
import hammerlab.show._
import ucar.ma2.DataType
import ucar.nc2.NetcdfFile

/**
 * Helpers for displaying various data-types
 */
trait show {
  implicit val showDataType: Show[DataType] = Show { _.toString }

  implicit def attributeLines: ToLines[Attribute] =
    ToLines {
      case Attribute.Vals(name, datatype, values) ⇒
        values.size match {
          case 1 ⇒
            s"$name ($datatype): ${values.head}"
          case n ⇒
            Lines(
              s"$name ($datatype; $n):",
              indent(
                if (n < 10)
                  (0 until n).map(values(_).toString)
                else
                  Lines(
                    (0 until 10).map(values(_).toString),
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
