package org.lasersonlab.zarr

import java.io.{ ByteArrayOutputStream, IOException, OutputStream }
import java.nio.ByteBuffer
import java.nio.ByteBuffer._
import java.util.zip.Deflater.DEFAULT_COMPRESSION
import java.util.zip.{ Deflater, DeflaterOutputStream, InflaterInputStream }

import cats.implicits._
import circe.Decoder.Result
import circe._
import circe.auto._
import caseapp.core.Error.UnrecognizedValue
import caseapp.core.argparser.{ ArgParser, SimpleArgParser }
import hammerlab.option._
import org.blosc.JBlosc
import org.blosc.JBlosc._
import shapeless.the
import Runtime.getRuntime

import org.hammerlab.shapeless.instances.InstanceMap
import org.lasersonlab.commons.IOUtils.toByteArray

import scala.concurrent.ExecutionContext

sealed trait Compressor {
  def apply(path: Path, sizeHint: Opt[Int] = Non)(implicit ec: ExecutionContext): F[Arr[Byte]]
  def apply(os: OutputStream, itemsize: Int): OutputStream
}
object Compressor {

  case class ZLib(level: Int = DEFAULT_COMPRESSION)
    extends Compressor {
    def apply(path: Path, sizeHint: Opt[Int] = Non)(implicit ec: ExecutionContext): F[Arr[Byte]] =
      path
        .stream()
        .map(new InflaterInputStream(_))
        .map(toByteArray(_))

    def apply(os: OutputStream, itemsize: Int): OutputStream =
      new DeflaterOutputStream(
        os,
        new Deflater(
          level
        )
      )
  }
  object ZLib {
    val regex = """zlib(?:\((\d)\))""".r
    object parse {
      def unapply(str: String): Option[ZLib] =
        str match {
          case regex(level) ⇒ Some(ZLib(level.toInt))
          case _ ⇒ scala.None
        }
    }
  }

  case object None
    extends Compressor {
    def apply(path: Path, sizeHint: Opt[Int] = Non)(implicit ec: ExecutionContext): F[Arr[Byte]] = path.read
    def apply(os: OutputStream, itemsize: Int): OutputStream = os
  }

  import Blosc._, CName._

  case class NumThreads(value: Int)
  object NumThreads {
    implicit def unwrap(n: NumThreads): Int = n.value
  }

  case class Blosc(
        cname: CName = lz4,
       clevel:   Int = 5,
      shuffle:   Int = 1,
    blocksize:   Int = 0
  )(
    implicit numThreads: NumThreads =
      NumThreads(
        math.min(
          8,
          getRuntime.availableProcessors
        )
      )
  )
  extends Compressor {

    val MAX_BUFFER_SIZE = (1 << 31) - 1

    def apply(path: Path, sizeHint: Opt[Int] = Non)(implicit ec: ExecutionContext): F[Arr[Byte]] = {

      path.read.map {
        arr ⇒
          val size = arr.length
          val src = ByteBuffer.wrap(arr)

          var expansions = 0
          var bufferSize = sizeHint.getOrElse(1 << 21)  // 2 MB

          var break = false
          var ret: Arr[Byte] = null
          while (!break) {
            val buffer = allocate(bufferSize)

            buffer.clear()
            val decompressed = JBlosc.decompressCtx(src, buffer, bufferSize, numThreads)
            if (decompressed <= 0) {
              if (bufferSize == MAX_BUFFER_SIZE) {
                throw new IOException(
                  s"Blosc decompression failed ($decompressed) on path $path with maximum buffer-size 2GB"
                )
              }
              val newBufferSize =
                min(
                  MAX_BUFFER_SIZE,
                  4L * bufferSize
                )

              println(
                s"WARN: increasing buffer from $bufferSize to $newBufferSize while decompressing $path"
              )
              bufferSize = newBufferSize
              expansions += 1
            } else if (expansions == 0) {
              ret = buffer.array()
              break = true
            } else {
              val arr = new Arr[Byte](decompressed)
              buffer.get(arr)
              ret = arr
              break = true
            }
          }

          ret
      }
    }

    def apply(os: OutputStream, itemsize: Int): OutputStream =
      new ByteArrayOutputStream() {
        override def close(): Unit = {
          super.close()
          val bytes = toByteArray
          apply(bytes, itemsize, os)
          os.close()
        }
      }

    def apply(
      bytes: Arr[Byte],
      itemsize: Int,
      os: OutputStream
    ):
      Unit
    = {
      // JBlosc requires you give it at least this much space
      val destLength =
        math.min(
          Integer.MAX_VALUE - OVERHEAD,
          bytes.length
        ) +
        OVERHEAD

      val dest = allocate(destLength)

      val srcLength = bytes.length
      val src = allocateDirect(srcLength)
      src.put(bytes)

      val compressed =
        JBlosc.compressCtx(
          clevel,
          shuffle,
          itemsize,
          src,
          srcLength,
          dest,
          destLength,
          cname,
          blocksize,
          numThreads
        )

      if (compressed > 0) {
        os.write(dest.array(), 0, compressed)
        os.close()
      } else if (compressed == 0)
        if (destLength > bytes.length)
          throw new IllegalStateException(
            s"Blosc buffer apparently too small ($destLength) for data of size ${bytes.length}"
          )
        else
          apply(
            bytes,
            itemsize,
            os
          )
      else
        throw new Exception(
          s"Blosc error compressing ${bytes.length} bytes into buffer of size $destLength"
        )
    }
  }

  object Blosc {
    sealed abstract class CName(implicit name: sourcecode.Name) {
      override val toString: String = name.value
    }
    case object    lz4   extends CName
    case object    lz4hc extends CName
    case object   zlib   extends CName
    case object   zstd   extends CName
    case object snappy   extends CName
    object CName {

      implicit def toName(cname: CName): String = cname.toString

      val instances = InstanceMap[CName]

      implicit val encoder: Encoder[CName] =
        new Encoder[CName] {
          def apply(a: CName): Json = Json.fromString(a.toString)
        }

      implicit val decoder: Decoder[CName] =
        new Decoder[CName] {
          def apply(c: HCursor): Result[CName] =
            c
              .value
              .as[String]
              .flatMap {
                str ⇒
                  instances
                    .get(str)
                    .fold[Result[CName]] {
                      Left(
                        DecodingFailure(
                          s"Unrecognized blosc cname: $str",
                          c.history
                        )
                      )
                    } {
                      Right(_)
                    }
              }
        }
    }

    val regex = """blosc(?:\((\d)\))""".r
    object parse {
      def unapply(s: String): Option[Blosc] =
        s match {
          case regex(level) ⇒
            Some(
              Option(level)
                .map(_.toInt)
                .fold(
                  Blosc()
                ) {
                  level ⇒
                    Blosc(clevel = level)
                }
            )
          case _ ⇒ scala.None
        }
    }
  }

  implicit val decoder: Decoder[Compressor] =
    new Decoder[Compressor] {
      override def apply(c: HCursor): Result[Compressor] =
        if (c.value.isNull)
          Right(None)
        else
          c
            .get[String]("id")
            .flatMap {
              case "blosc" ⇒ the[Decoder[Blosc]].apply(c)
              case  "zlib" ⇒ the[Decoder[ ZLib]].apply(c)
              case   null  ⇒ Right(None)
            }
    }

  implicit val encoder: Encoder[Compressor] =
    new Encoder[Compressor] {
      def apply(a: Compressor): Json =
        a match {
          case None ⇒ Json.Null
          case z:  ZLib ⇒ the[Encoder[ ZLib]].apply(z).mapObject(_ add ("id", Json.fromString( "zlib")))
          case b: Blosc ⇒ the[Encoder[Blosc]].apply(b).mapObject(_ add ("id", Json.fromString("blosc")))
        }
    }

  implicit val argParser: ArgParser[Compressor] =
    SimpleArgParser(
      "Compressor to use on zarr chunks",
      {
        case  ZLib.parse( zlib) ⇒ Right( zlib)
        case Blosc.parse(blosc) ⇒ Right(blosc)
        case             "none" ⇒ Right( None)
        case                  s ⇒  Left(UnrecognizedValue(s))
      }
    )
}
