package org.lasersonlab.zarr

sealed trait Compressor
object Compressor {

  case class ZLib(level: Int) extends Compressor

  import Blosc._
  case class Blosc(cname: CName, clevel: Int, shuffle: Int)
  object Blosc {
    sealed trait CName
    object CName {
      case object lzip extends CName
    }
  }
}
