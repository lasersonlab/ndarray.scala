package org.lasersonlab.netcdf

import hammerlab.lines._
import hammerlab.str._
import org.lasersonlab.netcdf.show._

import scala.collection.JavaConverters._

case class Group(
  name: String,
  attributes: Seq[Attribute],
  vars: Seq[Variable],
  groups: Seq[Group]
) {
  def group(name: Str): Group =
    groups
      .find(_.name == name.toString)
      .getOrElse {
        throw new IllegalArgumentException(
          s"No group found with name $name: ${groups.map(_.name).mkString(",")}"
        )
      }

  def array(name: Str): Variable =
    vars
      .find(_.name == name.toString)
      .getOrElse {
        throw new IllegalArgumentException(
          s"No variable found with name $name: ${vars.map(_.name).mkString(",")}"
        )
      }
}

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

