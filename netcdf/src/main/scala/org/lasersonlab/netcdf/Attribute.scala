package org.lasersonlab.netcdf

import ucar.ma2.DataType

sealed trait Attribute {
  type Value
  def name: String
  def datatype: DataType
  def values: Seq[Value]
  def size = values.size
}
object Attribute {
  type Aux[V] = Attribute { type Value = V }

  case class Vals[T](name: String, datatype: DataType, values: Seq[T])
    extends Attribute {
    type Value = T
  }

  implicit def unwrap[T](attribute: Aux[T]): Seq[T] = attribute.values

  implicit def apply(attribute: ucar.nc2.Attribute): Attribute = {
    def array[T]: Array[T] = attribute.getValues.get1DJavaArray(attribute.getDataType).asInstanceOf[Array[T]]
    attribute.getDataType match {
      case DataType.STRING                   ⇒ Vals(attribute.getShortName, attribute.getDataType, array[String])
      case DataType.   INT | DataType.UINT   ⇒ Vals(attribute.getShortName, attribute.getDataType, array[   Int])
      case DataType.  LONG | DataType.ULONG  ⇒ Vals(attribute.getShortName, attribute.getDataType, array[  Long])
      case DataType. SHORT | DataType.USHORT ⇒ Vals(attribute.getShortName, attribute.getDataType, array[ Short])
      case DataType.  BYTE | DataType.UBYTE  ⇒ Vals(attribute.getShortName, attribute.getDataType, array[  Byte])
      case DataType. FLOAT                   ⇒ Vals(attribute.getShortName, attribute.getDataType, array[ Float])
      case DataType.DOUBLE                   ⇒ Vals(attribute.getShortName, attribute.getDataType, array[Double])
      case c ⇒
        throw new UnsupportedOperationException(
          s"Conversion of attributes of type ${attribute.getDataType} not implemented"
        )
    }
  }
}
