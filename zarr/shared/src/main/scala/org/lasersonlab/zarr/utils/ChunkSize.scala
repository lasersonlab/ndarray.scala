package org.lasersonlab.zarr.utils

import hammerlab.bytes._
import hammerlab.math.utils._

case class ChunkSize(size: Int)
object ChunkSize {
  implicit def fromBytes(bytes: Bytes): ChunkSize =
    ChunkSize(
      bytes
        .bytes
        .safeInt
        .getOrElse {
          throw new IllegalArgumentException(s"Chunk size too large: ${Bytes.format(bytes)}")
        }
    )
}
