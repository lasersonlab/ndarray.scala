package org.lasersonlab.zarr

import java.nio.ByteBuffer

import org.lasersonlab.ndarray.Read

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
  }
}

sealed abstract class DType(override val toString: String)
object DType {
  case object     int extends DType("i")
  case object    bool extends DType("b")
  case object   float extends DType("f")
  case object  string extends DType("S")
  case object unicode extends DType("U")

  // not implemented (yet?):
  // - V: void *
  // - u: unsigned integer types (little awkward on JVM; maybe doable)
  // - U: Py_UNICODE
  // - m: timedelta
  // - M: datetime
  // - c: "complex floating point"
}

sealed abstract class DataType[T](
  order: ByteOrder,
  dType: DType,
  size: Int,
  read: (ByteBuffer, Int) ⇒ T
)
extends Read[T]
{
  override def toString = s"$order$dType$size"
  @inline def apply(buff: ByteBuffer, idx: Int): T =
    read(buff, idx)
}
object DataType {

  import ByteOrder._
  type Order = ByteOrder

  import DType.{
    float ⇒ f,
    string ⇒ s,
    _
  }

  import scala.Array.fill

  case class    i32(order: Endianness) extends DataType[   Int](order, int,    4, (buf, idx) ⇒ { buf.order(order); buf.getInt(4 * idx) })
  case class    i64(order: Endianness) extends DataType[  Long](order, int,    8, (buf, idx) ⇒ { buf.order(order); buf.getLong(8 * idx) })
  case class  float(order: Endianness) extends DataType[Double](order,   f,    4, (buf, idx) ⇒ { buf.order(order); buf.getFloat(4 * idx) })
  case class double(order: Endianness) extends DataType[Double](order,   f,    8, (buf, idx) ⇒ { buf.order(order); buf.getDouble(8 * idx) })
  case class string( size:        Int) extends DataType[String]( None,   s, size, (buf, idx) ⇒ {
    val arr = fill(size)(0.toByte)
    buf.get(arr, size * idx, size)
    arr.map(_.toChar).mkString
  })
}
