package org.lasersonlab.zlib

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.typedarray._

case class Deflater(level: Int = 5) {
  import Deflater.pako
  def apply(in: Array[Byte]): Array[Byte] = {
    val buffer =
      TypedArrayBuffer.wrap(
        pako
          .deflate(
            new Uint8Array(in.toJSArray)
          )
          .asInstanceOf[Uint8Array]
          .buffer
      )

    val length = buffer.remaining()
    val out = Array.fill(length)(0.toByte)
    buffer.get(out)

    out
  }
}
object Deflater {
  import scalajs.js.Dynamic.{ global ⇒ g }
  val pako = g.require("pako")
}
