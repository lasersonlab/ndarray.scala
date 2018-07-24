package org.lasersonlab.netcdf

import java.net.URI
import java.nio.file.FileSystems

import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import hammerlab.bytes._
import hammerlab.cli._
import hammerlab.option._
import hammerlab.path.Path
import hammerlab.reflect._
import hammerlab.show._
import org.hammerlab.paths.FileSystems.installedProviders

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

  val main =
    Main(
      new App(_) {
        setUserProject(opts.userProject)

        for {
          arg ← _args.args
          path = Path(arg)
          raf = new NioReadOnlyRandomAccessFile(path)
        } {
          println(show"${path.toString} ${Bytes.format(path.size)}")
        }
      }
    )
}
