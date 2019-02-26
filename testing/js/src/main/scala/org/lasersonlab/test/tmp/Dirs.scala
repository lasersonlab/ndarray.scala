package org.lasersonlab.test.tmp

import org.lasersonlab.files.Local
import org.lasersonlab.files.Local.fs

trait Dirs {
  self: Paths ⇒
  def tmpDir(prefix: String = this.getClass.getSimpleName): Path = {
    val dir = Local(fs.mkdtempSync(prefix).asInstanceOf[String])
    dirs += dir
    dir
  }
}

