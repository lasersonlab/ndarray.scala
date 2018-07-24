package org.lasersonlab.netcdf

import java.net.URI
import java.nio.file.FileSystems

import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import hammerlab.bytes._
import hammerlab.cli._
import hammerlab.indent.spaces
import hammerlab.lines._
import hammerlab.option._
import hammerlab.path.Path
import hammerlab.reflect._
import hammerlab.show._
import org.hammerlab.paths.FileSystems.installedProviders
import ucar.ma2.DataType
import ucar.nc2.{ Attribute, NetcdfFile }

import scala.collection.JavaConverters._


object Main
  extends Cmd {

  def setUserProject(userProject: Opt[String]): Unit = {
    for {
      userProject ← userProject
      gcsfs ← installedProviders.find(_.getScheme == "gs")
    } {
      gcsfs.set_!('userProject, userProject)
    }
  }

  case class Opts(
    @O("u") userProject: Option[String] = None
  )

  FileSystems.newFileSystem(
    new URI(s"s3:///"),
    Map.empty[String, String].asJava,
    Thread.currentThread().getContextClassLoader
  )

  implicit val showDataType: Show[DataType] = Show { _.toString }

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
            show"$name: ${ description.fold("") { d ⇒ show" ($d)" } } ($dtype, $size)",
            indent(
              show"dimensions ($rank): $dimensions",
              attrs
            )
          )
      }
  }

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
              indent(
                dimensions,
                attributes,
                label("vars", vars),
                label("groups", groups)
              )
            )

        if (name.nonEmpty)
          Lines(
            name,
            body
          )
        else
          body
      }
  }

  val main =
    Main(
      new App(_) {
        setUserProject(opts.userProject)

        for {
          arg ← _args.args
          path = Path(arg)
          _ = println(show"${path.toString} ${Bytes.format(path.size)}")
          raf = new NioReadOnlyRandomAccessFile(path)
          ncfile = NetcdfFile.open(raf, arg, null, null)
          root = ncfile.getRootGroup: Group
        } {
          printlns(root)
        }
      }
    )
}
