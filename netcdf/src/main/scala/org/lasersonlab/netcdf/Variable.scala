package org.lasersonlab.netcdf

import java.io.File
import java.net.{ URI, URL }

import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import hammerlab.lines._
import hammerlab.show._
import org.lasersonlab.netcdf.show._
import ucar.ma2.DataType
import ucar.nc2.{ CDMNode, NetcdfFile }

import scala.collection.JavaConverters._

case class Variable(
  name: String,
  description: Option[String],
  dtype: DataType,  // TODO: replace with our own DataType, for cross-compilation
  attrs: Seq[Attribute],
  dimensions: Seq[Dimension],
  rank: Int,
  shape: Seq[Int],
  size: Long,
  data: ucar.nc2.Variable
)

object Variable {
  /**
   * Re-open a [[ucar.nc2.Variable]] with a fresh [[NetcdfFile]] / [[NioReadOnlyRandomAccessFile]]; reading is not
   * thread-safe otherwise 😱
   */
  def copy(v: ucar.nc2.Variable) = {
    val file = {
      val file = v.getNetcdfFile
      val location = file.getLocation
      val uri = {
        val uri = new URI(location)
        if (uri.getScheme == null)
          new File(location).toURI
        else
          uri
      }
      NetcdfFile.open(
        new NioReadOnlyRandomAccessFile(uri),
        location.toString,
        null,
        null
      )
    }

    println(s"copying: ${v.getFullName}")

    def ancestors(node: CDMNode): Vector[String] =
      Option(node.getParentGroup)
        .filterNot(_.isRoot)
        .fold { Vector[String]() } { ancestors } :+
      node.getShortName

    val path = ancestors(v)
    println(s"traversing $path")
    path
      .toVector match {
        case groups :+ variable ⇒
          val group =
            groups
              .foldLeft(
                file.getRootGroup
              ) {
                (node, next) ⇒
                  node
                    .getGroups
                    .asScala
                    .find(_.getShortName == next)
                    .getOrElse {
                      throw new IllegalStateException(
                        s"Didn't find group $next in group ${node.getFullName} while trying to re-open ${v.getFullName}"
                      )
                    }
              }

          Option(group.findVariable(variable))
            .getOrElse {
              throw new IllegalStateException(
                s"Didn't find variable $variable in group ${group.getFullName} while trying to re-open ${v.getFullName}"
              )
            }
      }
  }

  implicit def apply(v: ucar.nc2.Variable): Variable =
    Variable(
      v.getShortName,
      Option(v.getDescription),
      v.getDataType,
      v.getAttributes.asScala.map { Attribute(_) },
      v.getDimensions.asScala.map { Dimension(_) },
      v.getRank,
      v.getShape,
      v.getSize,
      copy(v)
    )

  implicit val lines: ToLines[Variable] =
    ToLines {
      case Variable(
        name,
        description,
        dtype,
        attrs,
        dimensions,
        rank,
        _,
        size,
        _
      ) ⇒
        val descriptionString =
          description
          .filter(_.nonEmpty)
          .fold("") {
            d ⇒ show" ($d)"
          }
        Lines(
          show"$name:$descriptionString ($dtype, $size)",
          indent(
            show"dimensions ($rank): $dimensions",
            attrs
          )
        )
    }
}

