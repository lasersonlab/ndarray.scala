package org.lasersonlab.test

import hammerlab.collection._
import hammerlab.option._
import hammerlab.scalajs._
import lasersonlab.HasExecutionContext
import org.lasersonlab.files.Local

trait ResourceDirs
  extends HasExecutionContext {
  implicit def file: sourcecode.File

  def jvmBase = "jvm"
  def jsBase = "js"
  def sharedBase = "shared"
  def platformBase = jvmBase js_? jsBase

  def resourceDirs(
    dir: Local = Local.cwd
  ):
    List[Local] =
  {
    import Local.cwd
    println(s"cwd: $cwd")

    def candidates(dir: Local) =
      Stream(
        dir,
        dir / sharedBase,
        dir / platformBase
      )

    def expand(dir: Local) =
      candidates(dir)
        .map { _ / "src" / "test" / "resources" }
        .filter {
          dir ⇒
            val exists = dir.existsSync
            println(s"Checking dir $dir: $exists")
            exists
        }

    if (jvm_? && dir.basename == jvmBase)
      expand(dir.parent).toList
    else {
      (
        (jvm_? ? dir).toStream #:::
        Local(file.value)
        .unfold {
          _.parentOpt
        }
        .drop(1)
        .takeWhile(_ != dir)
        .reverse
     )
    .map { expand }
    .find { _.nonEmpty }
    .toList
    .flatten
    }
  }
}
