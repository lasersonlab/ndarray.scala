package org.lasersonlab.zlib

import org.lasersonlab.zlib.Deflater.pako

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.typedarray.{ ArrayBuffer, TypedArrayBuffer, Uint8Array }

case object Inflater {
  def apply(in: Array[Byte]): Array[Byte] = {
    val buffer =
      TypedArrayBuffer.wrap(
        pako
          .inflate(
            new Uint8Array(
              in.toJSArray
            )
          )
          .asInstanceOf[ArrayBuffer]
      )

    val length = buffer.remaining()
    val out = Array.fill(length)(0.toByte)
    buffer.get(out)

    out
  }
}
