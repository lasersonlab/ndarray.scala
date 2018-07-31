package org.lasersonlab.netcdf

import hammerlab.lines._
import hammerlab.show._
import org.lasersonlab.netcdf.show._
import ucar.ma2.DataType
import ucar.nc2.Attribute

import scala.collection.JavaConverters._

case class Variable(
  name: String,
  description: Option[String],
  dtype: DataType,
  attrs: Seq[Attribute],
  dimensions: Seq[Dimension],
  rank: Int,
  shape: Seq[Int],
  size: Long
)

object Variable {
  implicit def apply(v: ucar.nc2.Variable): Variable =
    Variable(
      v.getShortName,
      Option(v.getDescription),
      v.getDataType,
      v.getAttributes.asScala,
      v.getDimensions.asScala.map { Dimension(_) },
      v.getRank,
      v.getShape,
      v.getSize
    )

  implicit def lines: ToLines[Variable] =
    ToLines {
      case Variable(
        name,
        description,
        dtype,
        attrs,
        dimensions,
        rank,
        _,
        size
      ) ⇒
        Lines(
          show"$name:${ description.filter(_.nonEmpty).fold("") { d ⇒ show" ($d)" } } ($dtype, $size)",
          indent(
            show"dimensions ($rank): $dimensions",
            attrs
          )
        )
    }
}

