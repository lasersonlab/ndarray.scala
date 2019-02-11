package org.lasersonlab.uri

import java.io.{ ByteArrayInputStream, InputStream, OutputStream }
import java.net.URI
import java.nio.ByteBuffer
import java.util

import cats.implicits._
import _root_.io.circe.Decoder
import _root_.io.circe.parser.decode
import org.lasersonlab.io.FileNotFoundException
import org.lasersonlab.java_io.{ BoundedInputStream, SequenceInputStream }
import org.lasersonlab.uri.Uri.Segment
import slogging.LazyLogging

import scala.concurrent.ExecutionContext

// TODO: add (JVM-only) NIO Path implementation
// TODO: move block-caching to an implicit ctx-param that LRU-caches blocks across paths, and can centralize logic
//  around pre-fetching
// TODO: generalize F[_] type from [[Future]], apply to individual methods
abstract class Uri()(implicit val ec: ExecutionContext)
  extends LazyLogging
     with lasersonlab.future {

  type Self <: Uri

  val uri: URI
  def basename = uri.getPath.split("/").last
  def parentOpt: Option[Self]
  def parent   :        Self  = parentOpt.get

  def /(name: String): Self
  def /[T](name: T)(implicit s: Segment[T]): Self = /(s(name))

  def ?[T](basename: T)(implicit s: Segment[T]): F[Self] = {
    val path = this / basename
    path
      .exists
      .map {
        exists ⇒
          if (!exists)
            throw FileNotFoundException(path)
          else
            path
      }
  }

  val cachingConfig: caching.Config
  lazy val caching.Config(blockSize, maximumSize, maxNumBlocks) = cachingConfig

  override def toString: String = uri.toString

  def exists: F[Boolean]
  def isDirectory: Boolean

  def write(s: String): F[Unit] = write(s.getBytes())

  def write(bytes: Array[Byte]): F[Unit] = F {
    val os = outputStream
    os.write(bytes)
    os.close()
  }

  def outputStream: OutputStream = ???  // TODO: remove these defaults

  def delete: F[Unit] = ???
  def delete(recursive: Boolean = false): F[Unit] =
    if (recursive) {
      children
        .flatMap {
            _
              .map {
                _.delete(recursive = true)
              }
              .toList
              .sequence
          }
        .map {
          _ ⇒ delete()
        }
    } else
      delete

  def size: F[Long]

  def list: F[List[Self]] = children.map(_.toList)
  def children: F[Iterator[Self]]

  def blocks(from: Int = 0): F[List[Array[Byte]]] =
    getBlock(from)
      .flatMap {
        head ⇒
          if (head.length == blockSize)
            blocks(from + 1).map { head :: _ }
          else {
            logger.debug(s"$uri: got last block ($from; ${head.length})")
            (head :: Nil).pure[F]
          }
      }

  def read: F[Array[Byte]] =
    blocks()
      .map {
        case block :: Nil ⇒ block
        case blocks ⇒
          val bytes = Array.newBuilder[Byte]
          blocks.foreach { bytes ++= _ }
          bytes.result()
      }

  def bytes(start: Long, size: Int): F[Array[Byte]]

  def string: F[String] = read.map(new String(_))
  def string(start: Long, size: Int): F[String] = bytes(start, size).map(new String(_))

  def json[A](implicit d: Decoder[A]): F[A] =
    string
      .map[Either[Throwable, A]] {
        decode[A](_)
      }
      .rethrow

  // Stream from a starting point to the end of the file; continuously fetches+buffers blocks until the end fo the file,
  // whether or not previous blocks have been read / consumed
  //
  // TODO: make fetching behavior configurable: lazily load next block, or fetch when some portion of the existing block
  // as been consumed, etc.
  def stream(start: Long = 0): F[InputStream] =
  {
    val size = 2 * blockSize - (start % blockSize).toInt
    val end = start + size
    bytes(start, size)
      .flatMap {
        bytes ⇒
          val head = new ByteArrayInputStream(bytes)
          if (bytes.length == size)
            stream(end)
              .map {
                tail ⇒
                  SequenceInputStream(
                    head :: tail :: Nil
                  )
              }
          else
            (head: InputStream).pure[F]
      }
  }

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

                  if (idx == 0)
                    stream.skip(startBlockOffset)

                  if (idx + 1 == endIdx)
                    BoundedInputStream(stream, endBlockOffset)

                  stream
              }
          )
      }
  }

  private val _buffer = ByteBuffer.allocate(blockSize)

  // TODO: this is just re-fetching blocks on every access / not how you do caching with referential transparency
  val blocks =
    new util.LinkedHashMap[Long, F[Array[Byte]]](
      (maximumSize / blockSize).toInt,
      0.7f,
      true
    ) {
      override def removeEldestEntry(eldest: util.Map.Entry[Long, F[Array[Byte]]]): Boolean =
        if (size() >= maxNumBlocks) {
          logger.debug(s"Size ${size()} > max num blocks $maxNumBlocks (total size $maximumSize)")
          true
        } else
          false
    }

  def getBlock(idx: Long): F[Array[Byte]] =
    blocks.get(idx) match {
      case null ⇒
        val start = idx * blockSize
        logger.debug(s"fetching block $idx")
        val block = bytes(start, blockSize)
        blocks.put(idx, block)
        block
      case block ⇒ block
    }
}

object Uri {
  trait Segment[T] {
    def apply(t: T): String
  }
  object Segment {
    implicit val str: Segment[String] = (s: String) ⇒ s
    implicit val sym: Segment[Symbol] = _.name
    implicit val int: Segment[   Int] = _.toString
  }
  def apply(str: String)(implicit ec: ExecutionContext): Local = Local(str)
}
