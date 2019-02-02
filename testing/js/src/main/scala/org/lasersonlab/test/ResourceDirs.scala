package org.lasersonlab.test

import lasersonlab.HasExecutionContext
import org.lasersonlab.uri.Local

trait ResourceDirs
  extends HasExecutionContext {
  implicit def file: sourcecode.File

  val blacklist = Set(".git", ".idea")

  def resourceDirs(
    dir: Local = Local.cwd,
    maxDepth: Int = 2
  ):
    List[Local] =
    {
      val resources = dir / "src" / "test" / "resources"
      println(s"Looking for src/test/resources under $dir…")
      if (resources existsSync)
        List(resources)
      else
        Nil
    } ++ (
      if (maxDepth > 0)
        dir
          .childrenSync
          .filterNot(child ⇒ blacklist(child.basename))
          .filter(_.isDirectory)
          .flatMap { resourceDirs(_, maxDepth - 1) }
          .toList
      else
        Nil
    )
}
