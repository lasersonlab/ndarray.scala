package org.blosc

import java.nio.{ Buffer, ByteBuffer }

object JBlosc {
  val OVERHEAD = 16
  def   compressCtx(
    compressionLevel: Int,
         shuffleType: Int,
            typeSize: Int,
                 src: ByteBuffer,
           srcLength: Long,
                dest: ByteBuffer,
          destLength: Long,
      compressorName: String,
           blockSize: Int,
          numThreads: Int
  ): Int = ???

  def decompressCtx(
           src: Buffer,
          dest: Buffer,
      destSize: Long,
    numThreads: Int
  ): Int = ???
}
