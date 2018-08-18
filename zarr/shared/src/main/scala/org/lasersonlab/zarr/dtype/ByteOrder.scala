package org.lasersonlab.zarr.dtype

import org.lasersonlab.zarr.InstanceMap

import scala.PartialFunction.condOpt

sealed abstract class ByteOrder(override val toString: String)

object ByteOrder {

  sealed abstract class Endianness(override val toString: String)
    extends ByteOrder(toString)

  case object LittleEndian extends Endianness("<")
  case object    BigEndian extends Endianness(">")

  case object         None extends  ByteOrder("|")

  object Endianness {
    implicit def toByteOrder(endianness: Endianness): java.nio.ByteOrder =
      endianness match {
        case LittleEndian ⇒ java.nio.ByteOrder.LITTLE_ENDIAN
        case    BigEndian ⇒ java.nio.ByteOrder.BIG_ENDIAN
      }

    def unapply(char: Char): Option[Endianness] =
      condOpt(char) {
        case '<' ⇒ LittleEndian
        case '>' ⇒ BigEndian
      }
  }

  val get = InstanceMap[ByteOrder]()
}
