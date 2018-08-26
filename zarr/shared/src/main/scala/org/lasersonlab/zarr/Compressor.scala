package org.lasersonlab.zarr

import java.io.{ ByteArrayOutputStream, IOException }
import java.nio.ByteBuffer
import java.util.zip.{ DeflaterOutputStream, InflaterInputStream }

import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json }
import org.apache.commons.io.IOUtils
import org.blosc.JBlosc
import shapeless.the

import scala.util.Try
import scala.{ Array ⇒ Arr }

sealed trait Compressor {
  def apply(path: Path, sizeHint: Opt[Int] = Non): Arr[Byte]
  def apply(bytes: Arr[Byte], path: Path): Throwable | Unit
}
object Compressor {

  case class ZLib(level: Int) extends Compressor {
    def apply(path: Path, sizeHint: Opt[Int] = Non): Arr[Byte] = {
      val baos = new ByteArrayOutputStream(sizeHint.getOrElse(1 << 20))
      IOUtils.copy(new InflaterInputStream(path.inputStream), baos)
      baos.toByteArray
    }
    def apply(bytes: Arr[Byte], path: Path): Throwable | Unit =
      Try {
        val os = new DeflaterOutputStream(path.outputStream(mkdirs = true))
        os.write(bytes)
        os.close()
      }
      .toEither
  }

  case object None extends Compressor {
    def apply(path: Path, sizeHint: Opt[Int] = Non): Arr[Byte] = path.readBytes
    def apply(bytes: Arr[Byte], path: Path): Throwable | Unit =
      Try {
        val os = path.outputStream(mkdirs = true)
        os.write(bytes)
        os.close()
      }
      .toEither
  }

  import Blosc._

  case class Blosc(
    cname: CName,
    clevel: Int,
    shuffle: Int,
    blocksize: Int
  )
  extends Compressor {
    val blosc = new JBlosc
    blosc.setCompressor(cname.toString)

    val MAX_BUFFER_SIZE = (1 << 31) - 1

    def apply(path: Path, sizeHint: Opt[Int] = Non): Arr[Byte] = {

      val arr = path.readBytes
      val size = arr.length
      val src = ByteBuffer.wrap(arr)

      var expansions = 0
      var bufferSize = sizeHint.getOrElse(1 << 21)  // 2 MB
      while (true) {
        val buffer = ByteBuffer.allocate(bufferSize)

        buffer.clear()
        val decompressed = blosc.decompress(src, buffer, bufferSize)
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

    @inline def apply(
      bytes: Arr[Byte],
      path: Path
    ):
      Throwable | Unit
    =
      Try {
        withHint(
          bytes,
          path,
          bytes.length / 2,
          1
        )
      }
      .toEither

    private def withHint(
      bytes: Arr[Byte],
      path: Path,
      destSize: Int,
      attempts: Int
    ):
      Unit
    = {
      val dest = ByteBuffer.allocateDirect(destSize)
      val compressed =
        blosc.compress(
          clevel,
          shuffle,
          blocksize,
          ByteBuffer.wrap(bytes),
          bytes.length,
          dest,
          destSize
        )
      if (compressed > 0) {
        val os = path.outputStream(mkdirs = true)
        os.write(dest.array(), 0, compressed)
        os.close()
      } else if (compressed == 0)
        if (destSize > bytes.length)
          throw new IllegalStateException(
            s"Blosc buffer apparently too small ($destSize) for data of size ${bytes.length} after $attempts attempts"
          )
        else
          withHint(
            bytes,
            path,
            destSize * 2,
            attempts + 1
          )
      else
        throw new Exception(
          s"Blosc error compressing ${bytes.length} bytes into buffer of size $destSize on attempt $attempts"
        )
    }
  }

  object Blosc {
    sealed trait CName
    object CName {
      case object lz4 extends CName
      // TODO: others

      implicit val decoder: Decoder[CName] =
        new Decoder[CName] {
          def apply(c: HCursor): Result[CName] =
            c
              .value
              .as[String]
              .flatMap {
                case "lz4" ⇒ Right(lz4)
                case s ⇒
                  Left(
                    DecodingFailure(
                      s"Unrecognized blosc cname: $s",
                      c.history
                    )
                  )
              }
        }
    }
  }

  // TODO: add other Compressors

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
}
