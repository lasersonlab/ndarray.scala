package org.lasersonlab.test

import org.lasersonlab.uri.Local

trait ResourceDirs
  extends HasExecutionContext {
  def resourceDirs(
    dir: Local = Local(Local.cwd)
  ):
    List[Local] =
    (
      if (dir / "src" / "test" / "resources" existsSync)
        List(dir)
      else
        Nil
    ) ++ (
      if (dir.parent / "shared" / "src" / "test" / "resources" existsSync)
        List(dir.parent / "shared")
      else
        Nil
    )
}
