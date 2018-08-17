package org.lasersonlab.zarr

import java.nio.ByteBuffer

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.hammerlab.lines.Name
import org.lasersonlab.ndarray.io.Read
import org.lasersonlab.zarr.ByteOrder.Endianness

import PartialFunction.condOpt
import scala.util.Try

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

sealed abstract class DType(override val toString: String)
object DType {
  case object     int extends DType("i")
  case object    bool extends DType("b")
  case object   float extends DType("f")
  case object  string extends DType("S")

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

  trait Parser[T] {
    import Parser.Return
    def apply(str: String): Return[T]
  }
  object Parser {
    type Return[T] = String | DataType.Aux[T]
    def make[T](
      fn:
        PartialFunction[
          List[Char],
          DataType.Aux[T]
        ]
    )(
      implicit
      name: Name[T]
    ):
      Parser[T] =
      new Parser[T] {
        @inline def apply(str: String): Return[T] =
          fn
            .andThen(Right(_))
            .applyOrElse[List[Char], Return[T]](
              str.toList,
              (str: List[Char]) ⇒ Left(s"Unrecognized $name dtype: $str")
            )
      }

    object Int {
      def unapply(s: List[Char]): Option[Int] = Some( s.mkString.toInt )
    }

    implicit val   _char: Parser[  Char] = make { case           '|' :: 'i' :: Int(   1) ⇒   char       }
    implicit val     int: Parser[   Int] = make { case Endianness(e) :: 'i' :: Int(   4) ⇒    i32(   e) }
    implicit val    long: Parser[  Long] = make { case Endianness(e) :: 'i' :: Int(   8) ⇒    i64(   e) }
    implicit val  _float: Parser[ Float] = make { case Endianness(e) :: 'f' :: Int(   4) ⇒  float(   e) }
    implicit val _double: Parser[Double] = make { case Endianness(e) :: 'f' :: Int(   8) ⇒ double(   e) }
    implicit val _string: Parser[String] = make { case           '|' :: 'S' :: Int(size) ⇒ string(size) }
  }

  implicit def dataTypeDecoder[T](implicit parser: Parser[T]): Decoder[Aux[T]] =
    new Decoder[Aux[T]] {
      override def apply(c: HCursor): Result[Aux[T]] =
        c
          .value
          .as[String]
          .flatMap {
            parser(_)
              .left
              .map { DecodingFailure(_, c.history) }
          }
    }


  implicit def read[T](implicit dataType: DataType.Aux[T]): Read[T] =
    new Read[T] {
      @inline def apply(buff: ByteBuffer, idx: Int): T = dataType(buff, idx)
    }

  import ByteOrder._
  type Order = ByteOrder

  import DType.{ float ⇒ flt, string ⇒ str, _ }

  val `0` = 0.toByte

  // TODO: setting the buffer's order every time seems suboptimal; some different design that streamlines that would be nice
  case object   char                                 extends DataType( None, int,    1) { type T =   Char; @inline def apply(buf: ByteBuffer, idx: Int): T = {                   buf.getChar  (    idx) } }
  case  class    i32(override val order: Endianness) extends DataType(order, int,    4) { type T =    Int; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getInt   (4 * idx) } }
  case  class    i64(override val order: Endianness) extends DataType(order, int,    8) { type T =   Long; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getLong  (8 * idx) } }
  case  class  float(override val order: Endianness) extends DataType(order, flt,    4) { type T =  Float; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getFloat (4 * idx) } }
  case  class double(override val order: Endianness) extends DataType(order, flt,    8) { type T = Double; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getDouble(8 * idx) } }
  case  class string(override val  size:        Int) extends DataType( None, str, size) { type T = String
    import scala.Array.fill
    val arr = fill(size)(`0`)
    def apply(buf: ByteBuffer, idx: Int): T = {
      buf.position(size * idx)
      buf.get(arr)
      val builder = new StringBuilder
      var i = 0
      while (i < size) {
        val char = arr(i)
        if (char != `0`) builder += char.toChar
        else return builder.result()
        i += 1
      }
      builder.result()
    }
  }

  def get(order: ByteOrder, dtype: DType, size: Int): String | DataType =
    (order, dtype, size) match {
      case (e: Endianness, _: int.type,    4) ⇒ Right(   i32(   e))
      case (e: Endianness, _: int.type,    8) ⇒ Right(   i64(   e))
      case (e: Endianness, _: int.type,    1) ⇒ Right(  char      )
      case (e: Endianness, _: flt.type,    4) ⇒ Right( float(   e))
      case (e: Endianness, _: flt.type,    8) ⇒ Right(double(   e))
      case (         None, _: str.type, size) ⇒ Right(string(size))
      case _ ⇒
        Left(
          s"Unrecognized data type: $order$dtype$size"
        )
    }

  import DecodingFailure.fromThrowable
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
              fromThrowable(_, c.history)
            )
          (order, tpe, size) = t
             order ←     ByteOrder.get(order).left.map(DecodingFailure(_, c.history))
               tpe ←         DType.get(  tpe).left.map(DecodingFailure(_, c.history))
              size ← Try(size.toInt).toEither.left.map(  fromThrowable(_, c.history))
          datatype ←    get(order, tpe, size).left.map(DecodingFailure(_, c.history))
        } yield
          datatype
    }
}
