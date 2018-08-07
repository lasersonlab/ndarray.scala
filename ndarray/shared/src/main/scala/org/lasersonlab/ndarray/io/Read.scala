package org.lasersonlab.ndarray.io

import java.nio.ByteBuffer

trait Read[T] {
  /**
   * Read the record of type [[T]] at index `idx` in the array `bytes`
   *
   * `idx` is *not* the offset in the byte-array to start reading at; it refers to the `idx`-th record in the array of
   * serialized records; this leaves room for implementations that e.g. store records in variable numbers of bytes
   */
  def apply(buff: ByteBuffer, idx: Int): T
}
object Read {
  implicit val int: Read[Int] =
    new Read[Int] {
      val size = 4
      @inline def apply(buff: ByteBuffer, idx: Int): Int = buff.getInt(idx * size)
    }
}
