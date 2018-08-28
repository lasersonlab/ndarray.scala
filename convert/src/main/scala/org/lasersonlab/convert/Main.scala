package org.lasersonlab.convert

import java.net.URI
import java.nio.file.FileSystems.newFileSystem

import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import hammerlab.cli.Cmd
import hammerlab.path._
import org.lasersonlab.netcdf.{ Attribute, Group, Variable }
import org.lasersonlab.zarr
import org.lasersonlab.zarr.untyped.Metadata.Aux
import ucar.nc2.NetcdfFile

import scala.collection.JavaConverters._

object Main
  extends Cmd {
  case class Opts()
  val main =
    Main(
      new App(_) {
        // Initialize S3 Filesystem
        newFileSystem(
          new URI(s"s3:///"),
          Map.empty[String, String].asJava,
          Thread.currentThread().getContextClassLoader
        )

        val Seq(from, to) = args.args.map(Path(_))


        val file =
          NetcdfFile.open(
            new NioReadOnlyRandomAccessFile(from),
            from.toString,
            null,
            null
          )

        val hdf5Group: Group = file.getRootGroup

        def convertGroup(group: Group): zarr.untyped.Group =
          zarr.untyped.Group(
            hdf5Group
              .vars
              .map {
                case Variable(
                  name,
                  description,
                  dtype,
                  attrs,
                  dimensions,
                  rank,
                  shape,
                  size,
                  data
                ) ⇒
                  name →
                    new zarr.untyped.Array {
                      val datatype: zarr.dtype.DataType = ???
                      override type T = datatype.T
                      override def metadata: Aux[T] = ???
                      override def apply(idxs: Int*): T = ???
                    }
              }
              .toMap,
            hdf5Group
              .groups
              .map {
                g ⇒
                  g.name →
                    convertGroup(g)
              }
              .toMap
            // TODO: convert attributes
//            zarr.Attrs(
//              hdf5Group
//                .attributes
//                .map {
//                  case Attribute.Vals(name, datatype, values) ⇒
//
//                }
//            )
          )

        val zarrGroup = convertGroup(hdf5Group)
      }
    )
}
