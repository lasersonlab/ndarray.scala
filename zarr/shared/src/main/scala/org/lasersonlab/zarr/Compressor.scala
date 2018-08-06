package org.lasersonlab.zarr

import java.io.ByteArrayOutputStream
import java.util.zip.InflaterInputStream

import hammerlab.path._
import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }
import io.circe.parser._
import io.circe.generic.auto._

import org.apache.commons.io.IOUtils

sealed trait Compressor {
  def apply(path: Path): scala.Array[Byte]
}
object Compressor {

  case class ZLib(level: Int) extends Compressor {
    def apply(path: Path): scala.Array[Byte] = {
      val baos = new ByteArrayOutputStream()
      IOUtils.copy(new InflaterInputStream(path.inputStream), baos)
      baos.toByteArray
    }
  }

  case object None extends Compressor {
    def apply(path: Path): scala.Array[Byte] = path.readBytes
  }

  import Blosc._

  case class Blosc(
    cname: CName,
    clevel: Int,
    shuffle: Int,
    blocksize: Int
  )
  extends Compressor {
    def apply(path: Path): scala.Array[Byte] = ???
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
            case "blosc" ⇒ implicitly[Decoder[Blosc]].apply(c)
            case "zlib" ⇒ implicitly[Decoder[ZLib]].apply(c)
            case null ⇒ Right(None)
          }
    }
}
