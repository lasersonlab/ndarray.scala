package org.lasersonlab.netcdf

import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import hammerlab.bytes._
import hammerlab.cli._
import hammerlab.indent.tab
import hammerlab.lines._
import hammerlab.path._
import hammerlab.show._
import lasersonlab._
import org.lasersonlab.netcdf.show._
import ucar.nc2.NetcdfFile

object Main
  extends Cmd {

  case class Opts(
    @M("Set the 'userProject' parameter for requests to GCS 'requester-pays' buckets")
    @O("g") gcpUserProject: Option[String] = None
  )

  val main =
    Main(
      new App(_) {
        aws()
        gcp.userProject(opts.gcpUserProject)

        for {
          arg ← args
          path = Path(arg)
          _ = println(show"Inspecting: ${path.toString} ${Bytes.format(path.size)}")
          raf = new NioReadOnlyRandomAccessFile(path)
          ncfile = NetcdfFile.open(raf, arg, null, null)
        } {
          printlns(ncfile)
        }
      }
    )
}
