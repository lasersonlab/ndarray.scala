package org.lasersonlab.test

import org.lasersonlab.uri.Local

trait ResourceDirs
  extends HasExecutionContext {
  def resourceDirs(
    dir: Local = Local.cwd
  ):
    List[Local] =
    List(
      dir / "src" / "test" / "resources",
      dir.parent / "shared" / "src" / "test" / "resources"
    )
    .filter {
      _ existsSync
    }
}
