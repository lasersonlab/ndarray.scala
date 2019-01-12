package org.lasersonlab.test.tmp

import cats.implicits._
import org.lasersonlab.test.{ AfterAlls, FuturizeHook }
import org.lasersonlab.uri.Local
import utest._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext

trait Paths
  extends Dirs {
  self: TestSuite with AfterAlls with FuturizeHook ⇒

  type Path = Local
  implicit def ec: ExecutionContext

  val files = ArrayBuffer[Path]()
  val  dirs = ArrayBuffer[Path]()

  /**
   * Return a [[Path]] to a temporary file that has not yet been created.
   */
  def tmpPath(prefix: String = this.getClass.getSimpleName,
              suffix: String = ""): Path =
    tmpDir() / (prefix + suffix)

  def tmpPath(basename: String): Path = tmpDir() / basename

  afterAll {
    files.foreach(f ⇒ if (f.existsSync) f.deleteSync(    ))
     dirs.foreach(d ⇒ if (d.existsSync) d.deleteSync(true))
  }
}
