package org.lasersonlab.test

import lasersonlab.HasExecutionContext
import org.lasersonlab.uri.Local

trait ResourceDirs
  extends HasExecutionContext {
  implicit def file: sourcecode.File

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

  def resourceDirs(
    dir: Local = Local.cwd
  ):
  List[Local] =
  {
    import Local.cwd
    val dirs =
      unfoldT[Local](Local(file.value)) {
        _.parentOpt
      }
      .drop(1)
      .takeWhile(_ != cwd)
      .toList
      .reverse

    (
      dir ::
      dir.parent / "shared" ::
      dirs
    )
    .map { _ / "src" / "test" / "resources" }
    .filter {
      dir ⇒
        println(s"Checking dir $dir: ${dir.existsSync}")
        dir.existsSync
    }
  }
}
