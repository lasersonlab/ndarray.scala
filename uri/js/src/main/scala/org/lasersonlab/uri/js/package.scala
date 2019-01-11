package org.lasersonlab.uri

package object js {
  val node_? = {
    val window = scalajs.runtime.environmentInfo.global.window
    if (window.isInstanceOf[Unit]) {
      println(s"assuming node; window: $window")
      true
    } else {
      println(s"found window: $window")
      false
    }
  }
  val browser_? = !node_?
}
