package lasersonlab

import java.net.URI
import java.nio.file.FileSystemAlreadyExistsException
import java.nio.file.FileSystems.newFileSystem

import scala.collection.JavaConverters._

trait aws {
  def apply(): Unit =
    try {
      // Initialize S3 Filesystem
      newFileSystem(
        new URI(s"s3:///"),
        Map.empty[String, String].asJava,
        Thread.currentThread().getContextClassLoader
      )
    } catch {
      case e: FileSystemAlreadyExistsException ⇒
    }
}
object aws extends aws
