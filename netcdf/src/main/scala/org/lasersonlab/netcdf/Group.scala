package org.lasersonlab.netcdf

import hammerlab.lines._
import org.lasersonlab.netcdf.show._
import ucar.nc2.Attribute

import scala.collection.JavaConverters._

case class Group(
  name: String,
  attributes: Seq[Attribute],
  dimensions: Seq[Dimension],
  vars: Seq[Variable],
  groups: Seq[Group]
)

object Group {
  implicit def apply(g: ucar.nc2.Group): Group =
    Group(
      g.getShortName,
      g.getAttributes.asScala,
      g.getDimensions.asScala.map(Dimension(_)),
      g.getVariables .asScala.map( Variable(_)),
      g.getGroups    .asScala.map(    Group(_))
    )

  implicit val lines: ToLines[Group] =
    ToLines {
      case Group(
        name,
        attributes,
        dimensions,
        vars,
        groups
      ) ⇒
        val body =
          Lines(
            dimensions,
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

