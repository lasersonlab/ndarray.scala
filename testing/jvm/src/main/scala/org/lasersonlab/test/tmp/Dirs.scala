package org.lasersonlab.test.tmp

import java.nio.file.Files.createTempDirectory

import cats.implicits._
import org.lasersonlab.files.Local

trait Dirs {
  self: Paths ⇒
  def tmpDir(prefix: String = this.getClass.getSimpleName): Path = {
    val f = Local(createTempDirectory(prefix).toFile)
    dirs += f
    f
  }
}
