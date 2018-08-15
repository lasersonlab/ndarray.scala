package org.lasersonlab.zarr

import java.nio.ByteBuffer

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.hammerlab.shapeless.instances.Instances
import org.lasersonlab.ndarray.io.Read
import shapeless.{ HList, the }
import shapeless.ops.hlist.ToTraversable

import scala.reflect.ClassTag
import scala.util.Try

trait InstanceMap[T] {
  def apply(): Map[String, Either[String, T]]
}
object InstanceMap {

  def apply[T]()(implicit i: InstanceMap[T]) = i()

  implicit def instances[T, L <: HList](
    implicit
    i: Instances.Aux[T, L],
    ct: ClassTag[T],
    t: ToTraversable.Aux[L, List, T]
  ):
    InstanceMap[T] =
    new InstanceMap[T] {
      def apply(): Map[String, Either[String, T]] =
        i()
          .toList
          .map {
            o ⇒
              o.toString →
                Right(o)
          }
          .toMap
          .withDefault {
            k ⇒
              Left(
                s"Unrecognized ${ct.runtimeClass.getName}: $k"
              )
          }
    }
}

abstract class HasInstanceMap[T]()(
  implicit
  i: InstanceMap[T]
) {
  val byString: Map[String, Either[String, T]] = i()
//    i()
//      .toList
//      .map {
//        o ⇒
//          o.toString →
//            Right(o)
//      }
//      .toMap
//      .withDefault {
//        k ⇒
//          Left(
//            s"Unrecognized ${ct.runtimeClass.getName}: $k"
//          )
//      }
}

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

  val get = InstanceMap[ByteOrder]()
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

  val get = InstanceMap[DType]()
}

sealed abstract class DataType(
  val order: ByteOrder,
  val dType: DType,
  val size: Int
) {
  override val toString = s"$order$dType$size"
  type T
  def apply(buff: ByteBuffer, idx: Int): T
}
object DataType {

  type Aux[_T] = DataType { type T = _T }

  implicit def read[T](implicit dataType: DataType.Aux[T]): Read[T] =
    new Read[T] {
      @inline def apply(buff: ByteBuffer, idx: Int): T = dataType(buff, idx)
    }

  import ByteOrder._
  type Order = ByteOrder

  import DType.{
    float ⇒ f,
    string ⇒ str,
    _
  }

  import scala.Array.fill

  def make[_T](
    order: ByteOrder,
    dType: DType,
    size: Int,
    read: (ByteBuffer, Int) ⇒ _T
  ): Aux[_T] =
    new DataType(order, dType, size) {
      type T = _T
      @inline def apply(buff: ByteBuffer, idx: Int): T = read(buff, idx)
    }

  // TODO: setting the buffer's order every time seems suboptimal; some different design that streamlines that would be nice
  case object   char                                 extends DataType( None, int,    1) { type T =   Char; @inline def apply(buf: ByteBuffer, idx: Int): T = {                   buf.getChar  (    idx) } }
  case  class    i32(override val order: Endianness) extends DataType(order, int,    4) { type T =    Int; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getInt   (4 * idx) } }
  case  class    i64(override val order: Endianness) extends DataType(order, int,    8) { type T =   Long; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getLong  (8 * idx) } }
  case  class  float(override val order: Endianness) extends DataType(order,   f,    4) { type T =  Float; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getFloat (4 * idx) } }
  case  class double(override val order: Endianness) extends DataType(order,   f,    8) { type T = Double; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getDouble(8 * idx) } }
  case  class string(override val  size:        Int) extends DataType( None, str, size) { type T = String
    @inline def apply(buf: ByteBuffer, idx: Int): T = {
      val arr = fill(size)(0.toByte)
      buf.get(arr, size * idx, size)
      arr.map(_.toChar).mkString
    }
  }

  def get(order: ByteOrder, dtype: DType, size: Int): Either[String, DataType] =
    (order, dtype, size) match {
      case (e: Endianness, _: int.type,    4) ⇒ Right(   i32(   e))
      case (e: Endianness, _: int.type,    8) ⇒ Right(   i64(   e))
      case (e: Endianness, _: int.type,    1) ⇒ Right(  char      )
      case (e: Endianness, _:   f.type,    4) ⇒ Right( float(   e))
      case (e: Endianness, _:   f.type,    8) ⇒ Right(double(   e))
      case (         None, _: str.type, size) ⇒ Right(string(size))
      case _ ⇒
        Left(
          s"Unrecognized data type: $order$dtype$size"
        )
    }

  import org.hammerlab.lines.Name
  def makeDecoder[DT <: Aux[T], T](implicit name: Name[DT]): Decoder[DataType.Aux[T]] =
    new Decoder[Aux[T]] {
      override def apply(c: HCursor): Result[Aux[T]] =
        decoder(c)
        .flatMap {
          case dt: DT ⇒ Right(dt)
          case o ⇒
            Left(
              DecodingFailure(
                s"Expected $name data type, found $o",
                c.history
              )
            )
        }
    }

  implicit val stringDecoder: Decoder[DataType.Aux[String]] = makeDecoder[   string, String]
  implicit val   charDecoder: Decoder[DataType.Aux[  Char]] = makeDecoder[char.type,   Char]
  implicit val doubleDecoder: Decoder[DataType.Aux[Double]] = makeDecoder[   double, Double]
  implicit val  floatDecoder: Decoder[DataType.Aux[ Float]] = makeDecoder[    float,  Float]
  implicit val    intDecoder: Decoder[DataType.Aux[   Int]] = makeDecoder[      i32,    Int]
  implicit val   longDecoder: Decoder[DataType.Aux[  Long]] = makeDecoder[      i64,   Long]

  val regex = """(.)(.)(\d+)""".r
  implicit val decoder: Decoder[DataType] =
    new Decoder[DataType] {
      def apply(c: HCursor): Result[DataType] =
        for {
          s ← c.value.as[String]
          t ←
            Try {
              val regex(order, tpe, size) = s
              (order, tpe, size)
            }
            .toEither
            .left
            .map(
              DecodingFailure.fromThrowable(_, c.history)
            )
          (order, tpe, size) = t
          order ← ByteOrder.get(order).left.map(DecodingFailure(_, c.history))
          tpe ← DType.get(tpe).left.map(DecodingFailure(_, c.history))
          size ← Try(size.toInt).toEither.left.map(DecodingFailure.fromThrowable(_, c.history))
          datatype ← get(order, tpe, size).left.map(DecodingFailure(_, c.history))
        } yield
          datatype
    }
}
