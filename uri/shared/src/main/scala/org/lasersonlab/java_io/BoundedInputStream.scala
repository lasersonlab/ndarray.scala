package org.lasersonlab.java_io

import java.io.InputStream

import hammerlab.math.utils._

case class BoundedInputStream(is: InputStream, size: Long)
  extends InputStream {
  var pos = 0L
  override def read(): Int =
    if (pos == size)
      -1
    else {
      val b = is.read()
      pos += 1
      b
    }

  override def available(): Int =
    min(
      is.available(),
      size - pos
    )

  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    val n = is.read(b, off, min(len, size - pos))
    pos += n
    n
  }

  override def close(): Unit = is.close()
}
