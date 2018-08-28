package org.lasersonlab.netcdf

import hammerlab.lines._
import org.lasersonlab.netcdf.show._

import scala.collection.JavaConverters._

case class Group(
  name: String,
  attributes: Seq[Attribute],
  vars: Seq[Variable],
  groups: Seq[Group]
)

object Group {
  implicit def apply(g: ucar.nc2.Group): Group =
    if (g.getDimensions.isEmpty)
      Group(
        g.getShortName,
        g.getAttributes.asScala.map(Attribute(_)),
        g.getVariables .asScala.map( Variable(_)),
        g.getGroups    .asScala.map(    Group(_))
      )
    else
      throw new UnsupportedOperationException(
        s"Dimensions in NetCDF Groups not supported:\n${g.getDimensions.asScala.map(Dimension(_)).mkString(",")}\nGroup:\n$g"
      )

  implicit val lines: ToLines[Group] =
    ToLines {
      case Group(
        name,
        attributes,
        vars,
        groups
      ) ⇒
        val body =
          Lines(
            attributes,
            label("vars", vars),
            label("groups", groups)
          )

        if (name.nonEmpty)
          Lines(
            s"$name:",
            indent(body)
          )
        else
          body
    }
}

