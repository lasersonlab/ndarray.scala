package org.lasersonlab.convert

import hammerlab.bytes._
import hammerlab.cli._
import hammerlab.path._
import lasersonlab._
import org.lasersonlab.netcdf
import org.lasersonlab.zarr.Compressor
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.untyped

object Main
  extends Cmd {

  case class Opts(
    @O("c")  chunkSize:            Bytes =              64 MB  ,
    @O("z") compressor:       Compressor =              Blosc(),
    @R         gcpOpts: netcdf.Main.Opts =   netcdf.Main.Opts()
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

        aws()
        gcp.userProject(gcpUserProject)

        val Seq(from, to) = args.args.map(Path(_))

        val hdf5Group: netcdf.Group = from
        val zarrGroup: untyped.Group = hdf5Group

        println(s"${hdf5Group.vars.size} vars: ${zarrGroup.arrays.keys.mkString(",")}, ${hdf5Group.groups.size} groups: ${zarrGroup.groups.keys.mkString(",")}")
        println(s"Saving to: $to")

        zarrGroup
          .save(to)
          .left
          .foreach { throw _ }
      }
    )
}
