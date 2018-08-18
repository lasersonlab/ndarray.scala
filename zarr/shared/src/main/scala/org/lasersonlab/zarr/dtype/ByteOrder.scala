package org.lasersonlab.zarr.dtype

import org.lasersonlab.zarr.InstanceMap
import org.lasersonlab.zarr.dtype.ByteOrder._

import scala.PartialFunction.condOpt

sealed abstract class ByteOrder(override val toString: String)

trait Aliases {
  val < = LittleEndian
  val > = BigEndian
  val | = None
}

object ByteOrder
  extends Aliases {

  sealed abstract class Endianness(override val toString: String)
    extends ByteOrder(toString)

  implicit case object LittleEndian extends Endianness("<")
           case object    BigEndian extends Endianness(">")
           case object         None extends  ByteOrder("|")

  object implicits
    extends Aliases {
    implicit val BigEndian = ByteOrder.BigEndian
    implicit val None = ByteOrder.None
  }

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
