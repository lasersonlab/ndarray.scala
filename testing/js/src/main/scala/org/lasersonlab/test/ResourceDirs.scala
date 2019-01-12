package org.lasersonlab.test

import org.lasersonlab.uri.Local

trait ResourceDirs
  extends HasExecutionContext {
  def resourceDirs(
    dir: Local = Local(Local.cwd),
    maxDepth: Int = 2
  ):
    List[Local] =
    (
      if (dir / "src" / "test" / "resources" existsSync)
        List(dir)
      else
        Nil
    ) ++ (
      if (maxDepth > 0)
        dir
        .childrenSync
        .flatMap {
            child ⇒
              resourceDirs(child, maxDepth - 1)
          }
        .toList
      else
        Nil
    )
}
