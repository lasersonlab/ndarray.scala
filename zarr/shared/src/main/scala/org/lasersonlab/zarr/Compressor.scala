package org.lasersonlab.zarr

import java.io.{ ByteArrayOutputStream, IOException, OutputStream }
import java.nio.ByteBuffer
import java.nio.ByteBuffer._
import java.util.zip.Deflater.DEFAULT_COMPRESSION
import java.util.zip.{ Deflater, DeflaterOutputStream, InflaterInputStream }

import _root_.io.circe.Decoder.Result
import _root_.io.circe.generic.auto._
import _root_.io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json }
import caseapp.core.Error.UnrecognizedValue
import caseapp.core.argparser.{ ArgParser, SimpleArgParser }
import hammerlab.option._
import hammerlab.path._
import org.apache.commons.io.IOUtils
import org.blosc.JBlosc
import org.blosc.JBlosc._
import org.lasersonlab.zarr.Compressor.Blosc.CName.lz4
import shapeless.the
import Runtime.getRuntime

import org.hammerlab.shapeless.instances.InstanceMap

import scala.{ Array ⇒ Arr }

sealed trait Compressor {
  def apply(path: Path, sizeHint: Opt[Int] = Non): Arr[Byte]
  def apply(os: OutputStream, itemsize: Int): OutputStream
}
object Compressor {

  case class ZLib(level: Int = DEFAULT_COMPRESSION)
    extends Compressor {
    def apply(path: Path, sizeHint: Opt[Int] = Non): Arr[Byte] = {
      val baos = new ByteArrayOutputStream(sizeHint.getOrElse(1 << 20))
      IOUtils.copy(new InflaterInputStream(path.inputStream), baos)
      baos.toByteArray
    }
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
    def apply(path: Path, sizeHint: Opt[Int] = Non): Arr[Byte] = path.readBytes
    def apply(os: OutputStream, itemsize: Int): OutputStream = os
  }

  import Blosc._

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

    def apply(path: Path, sizeHint: Opt[Int] = Non): Arr[Byte] = {

      val arr = path.readBytes
      val size = arr.length
      val src = ByteBuffer.wrap(arr)

      var expansions = 0
      var bufferSize = sizeHint.getOrElse(1 << 21)  // 2 MB

      while (true) {
        val buffer = allocate(bufferSize)

        buffer.clear()
        val decompressed = JBlosc.decompressCtx(src, buffer, bufferSize, numThreads)
        if (decompressed < 0)
          throw new IOException(
            s"Internal error in Blosc decompression of $path (size: ${arr.length}) into buffer of size $bufferSize"
          )
        else if (decompressed == 0) {
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
        } else
          if (expansions == 0)
            return buffer.array()
          else {
            val arr = new Arr[Byte](decompressed)
            buffer.get(arr)
            return arr
          }
      }

      ???  // unreachable
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
        ) + OVERHEAD

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

    trait Named {
      override val toString: String =
        getClass
          .getSimpleName
          .filterNot(_ == '$')
    }

    sealed trait CName extends Named
    object CName {
      case object    lz4   extends CName
      case object    lz4hc extends CName
      case object   zlib   extends CName
      case object   zstd   extends CName
      case object snappy   extends CName

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
                  instances(str)
                    .left
                    .map {
                      _ ⇒
                        DecodingFailure(
                          s"Unrecognized blosc cname: $str",
                          c.history
                        )
                    }
              }
        }
    }
  }

  implicit val decoder: Decoder[Compressor] =
    new Decoder[Compressor] {
      override def apply(c: HCursor): Result[Compressor] =
        c
          .get[String]("id")
          .flatMap {
            case "blosc" ⇒ the[Decoder[Blosc]].apply(c)
            case  "zlib" ⇒ the[Decoder[ZLib]].apply(c)
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
        case ZLib.parse(zlib) ⇒ Right(zlib)
        case Blosc.parse(blosc) ⇒ Right(blosc)
        case "none" ⇒ Right(None)
        case s ⇒ Left(UnrecognizedValue(s))
      }
    )
}
