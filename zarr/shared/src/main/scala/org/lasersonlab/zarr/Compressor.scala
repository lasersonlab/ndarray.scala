package org.lasersonlab.zarr

import hammerlab.path._
import io.circe.Decoder.Result
import io.circe.{ Decoder, HCursor }

sealed trait Compressor {
  def apply(path: Path): scala.Array[Byte]
}
object Compressor {

  import Blosc._

  case class ZLib(level: Int) extends Compressor {
    def apply(path: Path): scala.Array[Byte] = ???
  }

  case object None extends Compressor {
    def apply(path: Path): scala.Array[Byte] = path.readBytes
  }

  case class Blosc(
    cname: CName,
    clevel: Int,
    shuffle: Int,
    blocksize: Int
  )
  extends Compressor {
    def apply(path: Path): scala.Array[Byte] = ???
  }

  // TODO: others

  object Blosc {
    sealed trait CName
    object CName {
      case object lz4 extends CName
      // TODO: others
    }
  }

  implicit val decoder: Decoder[Compressor] =
    new Decoder[Compressor] {
      override def apply(c: HCursor): Result[Compressor] = ???
    }
}
