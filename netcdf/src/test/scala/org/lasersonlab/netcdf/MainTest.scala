package org.lasersonlab.netcdf

import java.net.URI
import java.nio.ByteBuffer
import java.nio.file.{ FileSystems, Files, Paths }

import com.upplication.s3fs.S3Path

import scala.collection.JavaConverters._

class MainTest
  extends hammerlab.Suite {

  // TODO: set up AWS credentials for this test
  if (Option(System.getenv("TRAVIS")).isEmpty) {
    test("read s3 file") {

      val url = "s3:///matrix-format-test-data/matrices/loom/tenx_mouse_neuron_1M/loom_tenx_mouse_neuron_1M.loom"
      val uri = new URI(url)

      val fs =
        FileSystems.newFileSystem(
          new URI(s"s3:///"),
          Map.empty[String, String].asJava,
          Thread.currentThread().getContextClassLoader
        )

      val s3path = Paths.get(uri).asInstanceOf[S3Path]
      ==(
        Files.size(s3path),
        6666864827L
      )

      val n = 100
      val buffer = ByteBuffer.allocate(n)
      val ch = Files.newByteChannel(s3path)
      ==(ch.read(buffer), n)

      ==(
        buffer
          .array()
          .map("%2x".format(_))
          .grouped(4).map(_.mkString(" " ))
          .grouped(2).map(_.mkString("  "))
          .mkString("\n"),
        """89 48 44 46   d  a 1a  a
          | 0  0  0  0   0  8  8  0
          | 4  0 10  0   0  0  0  0
          | 0  0  0  0   0  0  0  0
          |ff ff ff ff  ff ff ff ff
          |bb 48 60 8d   1  0  0  0
          |ff ff ff ff  ff ff ff ff
          | 0  0  0  0   0  0  0  0
          |60  0  0  0   0  0  0  0
          | 1  0  0  0   0  0  0  0
          |88  0  0  0   0  0  0  0
          |a8  2  0  0   0  0  0  0
          | 1  0  7  0"""
          .stripMargin
      )
    }
  }
}
