package org.lasersonlab.netcdf

import java.net.URI
import java.nio.file.{ FileSystems, Files, Paths }

import com.amazonaws.services.s3.AmazonS3ClientBuilder
import com.amazonaws.services.s3.model.GetObjectRequest
import com.upplication.s3fs.S3Path

import scala.collection.JavaConverters._

class MainTest
  extends hammerlab.Suite {
  test("list bucket") {
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

    val bucket = s3path.getFileStore.name()
    val path = s3path.getKey

    val s3 = AmazonS3ClientBuilder.defaultClient
    //val uri = path.toUri
    //val bucket = uri.getAuthority

    val response = s3.getObject(new GetObjectRequest(bucket, path).withRange(0, 1000))
    val stream = response.getObjectContent

    val bytes = Array.fill(1000)(0.toByte)
    stream.read(bytes)

    val avilable = stream.available()

    //Files.newInputStream()

    //val ch = Files.newByteChannel(s3path)

  }
}
