package org.lasersonlab.zlib

import java.util.zip

import scala.Array.fill
import math.max

case class Deflater(level: Int = -1) {
  def apply(in: Array[Byte]): Array[Byte] = {
    val deflater = new zip.Deflater(level)
    deflater.setInput(in)
    deflater.finish()
    val out = fill(max(256, in.length))(0.toByte)
    val n = deflater.deflate(out)
    deflater.end()
    if (n == 0 || n == out.length)
      throw new IllegalStateException(
        s"Deflating ${in.length} bytes returned $n"
      )
    out.slice(0, n)
  }
}
