package org.lasersonlab.test

import java.util.MissingResourceException

import lasersonlab.HasExecutionContext
import org.lasersonlab.files.Local

trait Resources
 extends HasExecutionContext
    with ResourceDirs {
  /**
   * Override this to configure where tests will look for resources
   *
   * By default, any src/test/resources hierarchies rooted ≤2 levels below the current directory (to allow most
   * submodules' resource dirs to be detected)
   *
   * False-positives from having other modules' resources on the classpath shouldn't be a problem, in most cases 🤞.
   */
  lazy val resourceDirectories: List[Local] = resourceDirs()

  def resource(name: String): Local =
    resourceDirectories
      .flatMap {
        dir ⇒
          val resource = dir / name
          if (resource.existsSync)
            Some(resource)
          else
            None
      }
      .headOption
      .getOrElse {
        throw new MissingResourceException(
          s"No resource $name found in directories: ${resourceDirectories.mkString(",")}",
          getClass.getName,
          name
        )
      }
}
