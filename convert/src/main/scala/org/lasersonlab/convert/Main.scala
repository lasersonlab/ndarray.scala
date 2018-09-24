package org.lasersonlab.convert

import hammerlab.bytes._
import hammerlab.cli._
import hammerlab.path._
import lasersonlab._
import org.lasersonlab.netcdf
import org.lasersonlab.zarr.Compressor
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.utils.Idx

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

        val Seq(in, out) = args.args.map(Path(_))

        val idx = Idx.Int

        val  inGroup: netcdf.Group        = in
        val outGroup:   zarr.Group[idx.T] = convertGroup(inGroup)

        println(s"${inGroup.vars.size} vars: ${outGroup.arrays.keys.mkString(",")}, ${inGroup.groups.size} groups: ${outGroup.groups.keys.mkString(",")}")
        println(s"Saving to: $out")

        outGroup
          .save(out)
          .left
          .foreach { throw _ }
      }
    )
}
