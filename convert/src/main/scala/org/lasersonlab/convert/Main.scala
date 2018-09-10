package org.lasersonlab.convert

import java.net.URI
import java.nio.file.FileSystems.newFileSystem

import hammerlab.bytes._
import hammerlab.cli._
import hammerlab.path._
import org.lasersonlab.zarr.Compressor
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.group.Save
import org.lasersonlab.{ netcdf, zarr }

import scala.collection.JavaConverters._

object Main
  extends Cmd
     with Save.syntax {

  case class Opts(
    @O("c")  chunkSize:            Bytes =              64 MB  ,
    @O("z") compressor:       Compressor =              Blosc(),
    @R         gcpOpts: netcdf.Main.Opts =   netcdf.Main.Opts()
  )

  // Initialize S3 Filesystem
  newFileSystem(
    new URI(s"s3:///"),
    Map.empty[String, String].asJava,
    Thread.currentThread().getContextClassLoader
  )

  val main =
    Main(
      new App(_) {

        implicit val
          Opts(
            chunkSize,
            compressor,
            netcdf.Main.Opts(
              gcpUserProject
            )
          )
          = opts

        netcdf.Main.setGCPUserProject(gcpUserProject)

        val Seq(from, to) = args.args.map(Path(_))

        val hdf5Group: netcdf.Group = from
        val zarrGroup: zarr.untyped.Group = hdf5Group

        println(s"${hdf5Group.vars.size} vars: ${zarrGroup.arrays.keys.mkString(",")}, ${hdf5Group.groups.size} groups: ${zarrGroup.groups.keys.mkString(",")}")
        println(s"Saving to: $to")
        zarrGroup
          .save(to)
          .left
          .foreach { throw _ }
      }
    )
}
