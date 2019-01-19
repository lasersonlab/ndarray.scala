package org.lasersonlab.uri.caching

import hammerlab.either._
import hammerlab.math.utils._

case class Config(
  blockSize: Int,
  maxBlockCacheSize: Long,
  maxNumBlocks: Int
)

object Config {
  case class BlockSize(value: Int)
  case class MaxCacheSize(value: Long)

  implicit val    defaultBlockSize =    BlockSize( 2  << 20)
  implicit val defaultMaxCacheSize = MaxCacheSize(64L << 20)

  implicit def defaultConfig(
    implicit
    blockSize: BlockSize,
    maxCacheSize: MaxCacheSize
  ):
    Config =
    Config(
         blockSize.value,
      maxCacheSize.value
    )

  def apply(
    blockSize: Int,
    maxBlockCacheSize: Long
  ):
    Config =
    Config(
      blockSize,
      maxBlockCacheSize,
      (maxBlockCacheSize /↑ blockSize)
        .safeInt
        .getOrThrow
    )
}
