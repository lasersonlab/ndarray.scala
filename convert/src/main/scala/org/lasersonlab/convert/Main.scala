package org.lasersonlab.convert

import cats.implicits._
import hammerlab.cli._
import hammerlab.indent.spaces2
import hammerlab.lines._
import hammerlab.option._
import lasersonlab._
import lasersonlab.duration._
import lasersonlab.threads._
import lasersonlab.zarr._
import org.lasersonlab.netcdf
import org.lasersonlab.zarr.Compressor
import org.lasersonlab.zarr.Compressor.Blosc
import ucar.unidata.io.RandomAccessFile
import Runtime.getRuntime

import scala.concurrent.Await

object Main
  extends Cmd {

//  import NumThreads.wrapNumThreads

  case class Opts(
    @O("c")  chunkSize:            Bytes =              64 MB  ,
    @O("z") compressor:       Compressor =              Blosc(),
    @R             gcp: netcdf.Main.Opts =   netcdf.Main.Opts(),
    @O("t")    threads:       NumThreads =                cores,
    @O("w")    timeout:         Duration =               1.hour
  )

  val main =
    Main(
      new App(_) {

        RandomAccessFile.setGlobalFileCache(null)

        implicit val
          Opts(
            chunkSize,
            compressor,
            netcdf.Main.Opts(
              gcpUserProject
            ),
            threads,
            timeout
          )
          = opts

        implicit val ec = lasersonlab.threads(threads)

        aws()
        gcp.userProject(gcpUserProject)

        val Seq(in, out) = args.args.map(Path(_))

        val  inGroup: netcdf.Group = in
        val outGroup:   zarr.Group = inGroup

        println(s"${inGroup.vars.size} vars: ${outGroup.arrays.keys.mkString(",")}, ${inGroup.groups.size} groups: ${outGroup.groups.keys.mkString(",")}")
        println(s"Saving to: $out")

        Await.result(
          outGroup
            .save(out)
            .handleError { throw _ },
          1.hour
        )

        printlns(outGroup)
      }
    )
}
