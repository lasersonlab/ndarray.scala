package org.lasersonlab.uri

import java.io.{ ByteArrayInputStream, InputStream }
import java.net.URI
import java.nio.ByteBuffer
import java.util

import cats.effect.Sync
import cats.implicits._
import hammerlab.either._
import hammerlab.math.utils._
import io.circe.{ Decoder, DecodingFailure }
import io.circe.parser.decode
import org.lasersonlab.java_io.{ BoundedInputStream, SequenceInputStream }
import slogging.LazyLogging

case class Config(blockSize: Int, maxBlockCacheSize: Long, maxNumBlocks: Int)
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

abstract class Uri[F[_]: Sync]
  extends LazyLogging {
  import logger.debug

  val sync = Sync[F]
  @inline def delay[A](thunk: => A): F[A] = sync.delay(thunk)

  val uri: URI

  val config: Config
  lazy val Config(blockSize, maximumSize, maxNumBlocks) = config

  override def toString: String = uri.toString

  def exists: F[Boolean]

  def  size: F[Long]
  def _size: F[Int] = size.flatMap { size ⇒ delay { size.safeInt.getOrThrow } }

  def read: F[Array[Byte]] = _size.flatMap(size ⇒ bytes(0, size))

  def bytes(start: Long, size: Int): F[Array[Byte]]

  def string: F[String] = _size.flatMap(size ⇒ string(0, size))
  def string(start: Long, size: Int): F[String] = bytes(start, size).map(new String(_))

  def json[A](implicit d: Decoder[A]): F[A] =
    string
      .map[Either[Throwable, A]] {
        decode[A](_)
      }
      .rethrow

  def stream: F[InputStream] = size.flatMap(size ⇒ stream(0, size))
  def stream(start: Long, end: Long): F[InputStream] = {

    val startIdx =    start  / blockSize
    val   endIdx = (end - 1) / blockSize + 1

    val startBlockOffset = start % blockSize toInt
    val   endBlockOffset =   end % blockSize toInt

    (startIdx until endIdx)
      .toList
      .map(getBlock)
      .sequence
      .map {
        chunks ⇒
          SequenceInputStream(
            chunks
              .mapWithIndex {
                case (chunk, idx) ⇒
                  val stream = new ByteArrayInputStream(chunk)
                  if (idx + 1 == endIdx) {
                    BoundedInputStream(stream, endBlockOffset)
                  } else {
                    val stream = new ByteArrayInputStream(chunk)
                    if (idx == 0)
                      stream.skip(startBlockOffset)

                    stream
                  }
              }
          )

      }
  }

  private val _buffer = ByteBuffer.allocate(blockSize)

  val blocks =
    new util.LinkedHashMap[Long, F[Array[Byte]]](
      (maximumSize / blockSize).toInt,
      0.7f,
      true
    ) {
      override def removeEldestEntry(eldest: util.Map.Entry[Long, F[Array[Byte]]]): Boolean =
        if (size() >= maxNumBlocks) {
          debug(s"Size ${size()} > max num blocks $maxNumBlocks (total size $maximumSize)")
          true
        } else
          false
    }

  def getBlock(idx: Long): F[Array[Byte]] =
    if (!blocks.containsKey(idx)) {
      for {
        size ← size
        start = idx * blockSize
        fetchSize = min(blockSize, size - start)
        bytes ←
          blocks
            .put(
              idx,
              bytes(
                start,
                fetchSize
              )
            )
        _ = debug(s"Fetched block $idx: [$start,${start + size})")
      } yield
        bytes
    } else
      blocks.get(idx)
}
