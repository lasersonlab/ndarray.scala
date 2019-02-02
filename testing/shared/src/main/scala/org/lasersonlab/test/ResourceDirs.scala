package org.lasersonlab.test

import hammerlab.option._
import hammerlab.scalajs._
import lasersonlab.HasExecutionContext
import org.lasersonlab.uri.Local

trait ResourceDirs
  extends HasExecutionContext {
  implicit def file: sourcecode.File

  // TODO: move to collections lib
  def unfold[T1, T2](x: T1)(fn: T1 ⇒ Option[(T2, T1)]): Stream[T2] =
    fn(x) match {
      case None ⇒ Stream()
      case Some((result, next)) ⇒ result #:: unfold(next)(fn)
    }

  def unfoldT[T](x: T)(fn: T ⇒ Option[T]): Stream[T] =
    fn(x) match {
      case None ⇒ Stream()
      case Some(next) ⇒ next #:: unfoldT(next)(fn)
    }

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
        unfoldT[Local](Local(file.value)) {
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
