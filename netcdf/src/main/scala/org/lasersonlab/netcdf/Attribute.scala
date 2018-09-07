package org.lasersonlab.netcdf

import ucar.ma2.DataType
import ucar.ma2.DataType._

sealed trait Attribute {
  type Value
  def name: String
  def datatype: DataType
  def values: Seq[Value]
  def size = values.size
}
object Attribute {
  type Aux[V] = Attribute { type Value = V }

//  case class Vals[T](name: String, datatype: DataType, values: Seq[T])
//    extends Attribute {
//    type Value = T
//  }

  sealed abstract class Attr[V](val datatype: DataType) extends Attribute {
    type Value = V
  }

  case class   Chars(name: String, values: Seq[  Char]) extends Attr[  Char](  CHAR)
  case class   Bytes(name: String, values: Seq[  Byte]) extends Attr[  Byte](  BYTE)
  case class  Shorts(name: String, values: Seq[ Short]) extends Attr[ Short]( SHORT)
  case class    Ints(name: String, values: Seq[   Int]) extends Attr[   Int](   INT)
  case class   Longs(name: String, values: Seq[  Long]) extends Attr[  Long](  LONG)
  case class  Floats(name: String, values: Seq[ Float]) extends Attr[ Float]( FLOAT)
  case class Doubles(name: String, values: Seq[Double]) extends Attr[Double](DOUBLE)
  case class Strings(name: String, values: Seq[String]) extends Attr[String](STRING)

  implicit def unwrap[T](attribute: Aux[T]): Seq[T] = attribute.values

  implicit def apply(attribute: ucar.nc2.Attribute): Attribute = {
    def array[T]: Array[T] =
      attribute
        .getValues
        .get1DJavaArray(attribute.getDataType)
        .asInstanceOf[Array[T]]

    val name = attribute.getShortName
    attribute.getDataType match {
      case STRING ⇒ Strings(name, array[String])
      case   CHAR ⇒   Chars(name, array[  Char])
      case   BYTE ⇒   Bytes(name, array[  Byte])
      case  SHORT ⇒  Shorts(name, array[ Short])
      case    INT ⇒    Ints(name, array[   Int])
      case   LONG ⇒   Longs(name, array[  Long])
      case  UBYTE ⇒   Bytes(name, array[  Byte].map { n ⇒ if (n < 0) throw new IllegalArgumentException(s"Unsigned byte out of range: $n") else n })
      case USHORT ⇒  Shorts(name, array[ Short].map { n ⇒ if (n < 0) throw new IllegalArgumentException(s"Unsigned short out of range: $n") else n })
      case   UINT ⇒    Ints(name, array[   Int].map { n ⇒ if (n < 0) throw new IllegalArgumentException(s"Unsigned int out of range: $n") else n })
      case  ULONG ⇒   Longs(name, array[  Long].map { n ⇒ if (n < 0) throw new IllegalArgumentException(s"Unsigned long out of range: $n") else n })
      case  FLOAT ⇒  Floats(name, array[ Float])
      case DOUBLE ⇒ Doubles(name, array[Double])
      case c ⇒
        throw new UnsupportedOperationException(
          s"Conversion of attributes of type ${attribute.getDataType} not implemented"
        )
    }
  }
}
