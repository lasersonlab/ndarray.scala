package org.lasersonlab.test.tmp

import java.nio.file.Files.{ createTempDirectory, createTempFile }

import cats.implicits._
import org.lasersonlab.test.Hooks.F
import org.lasersonlab.test.{ AfterAlls, FuturizeHook }
import org.lasersonlab.uri.Local
import utest._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext

trait Paths {
  self: TestSuite with AfterAlls with FuturizeHook ⇒

  type Path = Local
  implicit def ec: ExecutionContext

  val files = ArrayBuffer[Path]()
  val  dirs = ArrayBuffer[Path]()

  /**
   * Create a temporary file and return a [[Path]] to it.
   */
  def tmpFile(prefix: String = this.getClass.getSimpleName, suffix: String = ""): Path = {
    val f = Local(createTempFile(prefix, suffix).toFile)
    files += f
    f
  }

  def tmpDir(prefix: String = this.getClass.getSimpleName): Path = {
    val f = Local(createTempDirectory(prefix).toFile)
    dirs += f
    f
  }

  /**
   * Return a [[Path]] to a temporary file that has not yet been created.
   */
  def tmpPath(prefix: String = this.getClass.getSimpleName,
              suffix: String = ""): Path =
    tmpDir() / (prefix + suffix)

  def tmpPath(basename: String): Path = tmpDir() / basename

  afterAll {
    (
      files.map(f ⇒ if (f.existsSync) f.delete(    ) else F { () }) ++
       dirs.map(d ⇒ if (d.existsSync) d.delete(true) else F { () })
    )
    .toList
    .sequence
    .map { _ ⇒ () }
  }
}
