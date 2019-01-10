package org.lasersonlab.uri

package object js {
  val node_? =
    try {
      scalajs.runtime.environmentInfo.global.window
      println(s"window: ${scalajs.runtime.environmentInfo.global.window}")
      false
    } catch {
      case e: Exception ⇒
        println(s"caught jsenv: $e")
        true
    }
  val browser_? = !node_?
}
