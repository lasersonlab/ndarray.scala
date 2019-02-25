package org.lasersonlab.java_io

import java.io.InputStream

/**
 * Wrap a [[List]] of [[InputStream]]s as an [[InputStream]]
 *
 * Re-/Alternate implementation of [[java.io.SequenceInputStream]]; works on JVM and JS, has a more Scala-y / cleaner
 * constructor
 */
case class SequenceInputStream(var remaining: List[InputStream])
  extends InputStream {
  var stream: InputStream = _

  private def nextStream(): Unit =
    if (remaining.nonEmpty) {
      stream = remaining.head
      remaining = remaining.tail
    }

  override def read(b: Array[Byte], off: Int, len: Int): Int =
    if (stream == null) {
      nextStream()
      if (stream == null)
        -1
      else
        read(b, off, len)
    } else {
      val n = stream.read(b, off, len)
      if (n == -1) {
        stream.close()
        stream = null
        nextStream()
        read(b, off, len)
      } else
        n
    }

  override def read(): Int =
    if (stream == null) {
      nextStream()
      read()
    } else {
      val n = stream.read()
      if (n == -1) {
        nextStream()
        read()
      } else
        n
    }


  override def available(): Int =
    if (stream == null)
      0
    else
      stream.available()


  override def close(): Unit = {
    if (stream != null)
      stream.close()
    remaining.foreach(_.close)
  }
}
