package org.lasersonlab.netcdf

import java.net.URI
import java.nio.file.FileSystems

import com.google.cloud.storage.contrib.nio.CloudStorageConfiguration
import com.google.cloud.storage.contrib.nio.CloudStorageFileSystemProvider.setDefaultCloudStorageConfiguration
import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import hammerlab.bytes._
import hammerlab.cli._
import hammerlab.indent.tab
import hammerlab.lines._
import hammerlab.option._
import hammerlab.path.Path
import hammerlab.show._
import org.lasersonlab.netcdf.show._
import ucar.nc2.NetcdfFile

import scala.collection.JavaConverters._

object Main
  extends Cmd {

  def setGCPUserProject(userProject: Opt[String]): Unit = {
    for {
      userProject ← userProject
    } {
      setDefaultCloudStorageConfiguration(
        CloudStorageConfiguration
          .builder()
          .userProject(userProject)
          .build()
      )
    }
  }

  case class Opts(
    @M("Set the 'userProject' parameter for requests to GCS 'requester-pays' buckets")
    @O("g") gcpUserProject: Option[String] = None
  )

  val main =
    Main(
      new App(_) {
        setGCPUserProject(opts.gcpUserProject)

        // Initialize S3 Filesystem
        FileSystems.newFileSystem(
          new URI(s"s3:///"),
          Map.empty[String, String].asJava,
          Thread.currentThread().getContextClassLoader
        )

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
