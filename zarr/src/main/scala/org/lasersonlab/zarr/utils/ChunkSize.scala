package org.lasersonlab.zarr.utils

import hammerlab.bytes._

case class ChunkSize(size: Int)
object ChunkSize {
  implicit def fromBytes(bytes: Bytes): ChunkSize =
    ChunkSize(
      bytes.bytes match {
        case n if n > Integer.MAX_VALUE ⇒ throw new IllegalArgumentException(s"Chunk size too large: ${Bytes.format(bytes)}")
        case n ⇒ n.toInt
      }
    )
}
