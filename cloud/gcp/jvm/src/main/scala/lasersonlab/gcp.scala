package lasersonlab

import java.net.URI

import com.google.cloud.storage.contrib.nio.CloudStorageFileSystemProvider._
import com.google.cloud.storage.contrib.nio._
import hammerlab.opt._
import hammerlab.reflect._

trait gcp {
  def userProject(userProject: ?[String]): Unit =
    for {
      userProject ← userProject
    } {
      println(s"Using user-project $userProject for GCP requests")
      setDefaultCloudStorageConfiguration(
        CloudStorageConfiguration
          .builder()
          .userProject(userProject)
          .build()
      )

      /**
       * in case a GCP FileSystemProvider was already created/cached by the SPI machinery, dig in and set its
       * `userProject` field as well
       */
      java.nio.file.FileSystems
        .getFileSystem(
          new URI("gs://bucket/")
        )
        .provider()
        .asInstanceOf[CloudStorageFileSystemProvider]
        .set_!('userProject, userProject)
    }
}
object gcp extends gcp
