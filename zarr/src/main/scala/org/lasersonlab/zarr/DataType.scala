package org.lasersonlab.zarr

sealed abstract class ByteOrder(override val toString: String)
object ByteOrder {

  sealed abstract class Endianness(override val toString: String)
    extends ByteOrder(toString)

  case object LittleEndian extends Endianness("<")
  case object    BigEndian extends Endianness(">")

  case object         None extends ByteOrder("|")
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
  // - m: timedelta
  // - M: datetime
  // - c: "complex floating point"
}

sealed abstract class DataType[T](
  order: ByteOrder,
  dType: DType,
  size: Int
) {
  override def toString = s"$order$dType$size"
  def apply(bytes: Array[Byte], pos: Int): T = ???
}
object DataType {

  import ByteOrder._
  type Order = ByteOrder

  import DType.{ float ⇒ f, string ⇒ s, _ }

  case class    i32(order: Endianness) extends DataType[   Int](order, int,    4)
  case class    i64(order: Endianness) extends DataType[   Int](order, int,    8)
  case class  float(order: Endianness) extends DataType[Double](order,   f,    4)
  case class double(order: Endianness) extends DataType[Double](order,   f,    8)
  case class string( size:        Int) extends DataType[String]( None,   s, size)
}
