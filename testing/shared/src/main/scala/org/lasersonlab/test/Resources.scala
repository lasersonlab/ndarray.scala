package org.lasersonlab.test

import java.io.File
import java.util.MissingResourceException

import org.lasersonlab.uri.Local

trait Resources
 extends HasExecutionContext {
  def resourceDirs(
    dir: Local = Local(new File(".").getCanonicalFile),
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
          .listSync
          .flatMap {
            child ⇒
              resourceDirs(child, maxDepth - 1)
          }
          .toList
      else
        Nil
    )

  /**
   * Override this to configure where tests will look for resources
   *
   * By default, any src/test/resources hierarchies rooted ≤2 levels below the current directory (to allow most
   * submodules' resource dirs to be detected)
   *
   * False-positives from having other modules' resources on the classpath shouldn't be a problem, in most cases 🤞.
   */
  def resourceDirectories = resourceDirs()

  def resource(name: String) =
    resourceDirectories
      .find {
        dir ⇒
          val resource = Local(s"$dir/$name")
          if (resource.existsSync)
            Some(resource)
          else
            None
      }
      .getOrElse {
        throw new MissingResourceException("", getClass.getName, name)
      }
}
