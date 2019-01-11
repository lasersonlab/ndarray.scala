package org.lasersonlab.test.tmp

import org.lasersonlab.test.Suite
import org.lasersonlab.uri.Local
import Local.fs

import scala.collection.mutable.ArrayBuffer

trait Paths {
  self: Suite ⇒

  type Path = Local

  val files = ArrayBuffer[Path]()
  val  dirs = ArrayBuffer[Path]()

  def tmpDir(prefix: String = this.getClass.getSimpleName): Path = {
    val dir = Local(fs.mkdtempSync(prefix).asInstanceOf[String])
    dirs += dir
    dir
  }

  /**
   * Return a [[Path]] to a temporary file that has not yet been created.
   */
  def tmpPath(prefix: String = this.getClass.getSimpleName,
              suffix: String = ""): Path =
    tmpDir() / (prefix + suffix)

  def tmpPath(basename: String): Path = tmpDir() / basename

  afterAll {
    files.foreach(f ⇒ if (f.exists) f.delete())
     dirs.foreach(d ⇒ if (d.exists) d.delete(true))
  }
}

