package org.lasersonlab.zarr

import java.nio.ByteBuffer

import io.circe.Decoder.Result
import io.circe.{ ACursor, Decoder, DecodingFailure, HCursor, Json }
import org.hammerlab.lines.Name
import org.lasersonlab.ndarray.io.Read
import org.lasersonlab.zarr.ByteOrder.Endianness
import shapeless.HNil

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

sealed abstract class DataType {
  def size: Int
  type T
  def apply(buff: ByteBuffer, idx: Int): T
}

trait StructDerivations {
  import shapeless._

  type Aux[_T] = DataType { type T = _T }

  implicit val hnil: Aux[HNil] =
    new DataType {
      type T = HNil
      val size = 0
      def apply(buff: ByteBuffer, idx: Int): HNil = HNil
    }

  implicit def cons[Head, Tail <: HList](
    implicit
    head: Lazy[Aux[Head]],
    tail: Lazy[Aux[Tail]]
  ): Aux[Head :: Tail] =
    new DataType {
      type T = Head :: Tail
      val size = head.value.size + tail.value.size
      def apply(buff: ByteBuffer, idx: Int): Head :: Tail = {
        buff.position(idx * size)
        val h = head.value(buff, 0)
        val t = tail.value(buff, 0)
        h :: t
      }
    }

  implicit def struct[
     S,
     L <: HList
  ](
    implicit
    g: Generic.Aux[S, L],
    l: Lazy[Aux[L]]
  ):
    Aux[S] =
    new DataType {
      val size = l.value.size
      type T = S
      def apply(buff: ByteBuffer, idx: Int): S = g.from(l.value(buff, idx))
    }
}

import Parser.Return

trait Parser[T] {
  def apply(c: HCursor): Return[T]
}
object Parser {
  type Return[T] = DecodingFailure | DataType.Aux[T]

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
      @inline def apply(c: HCursor): Return[T] =
        c
          .value
          .as[String]
          .flatMap {
            str ⇒
              fn
                .andThen(Right(_))
                .applyOrElse[List[Char], Return[T]](
                  str.toList,
                  str ⇒
                    Left(
                      DecodingFailure(
                        s"Unrecognized $name dtype: $str",
                        c.history
                      )
                    )
                )
          }
    }

  import DataType._

  implicit val   _char: Parser[  Char] = make { case           '|' :: 'i' :: Int(   1) ⇒   char       }
  implicit val     int: Parser[   Int] = make { case Endianness(e) :: 'i' :: Int(   4) ⇒    i32(   e) }
  implicit val    long: Parser[  Long] = make { case Endianness(e) :: 'i' :: Int(   8) ⇒    i64(   e) }
  implicit val  _float: Parser[ Float] = make { case Endianness(e) :: 'f' :: Int(   4) ⇒  float(   e) }
  implicit val _double: Parser[Double] = make { case Endianness(e) :: 'f' :: Int(   8) ⇒ double(   e) }
  implicit val _string: Parser[String] = make { case           '|' :: 'S' :: Int(size) ⇒ string(size) }

  import shapeless._

  implicit def structParser[S, L <: HList](
    implicit
    g: Generic.Aux[S, L],
    l: StructParser[L]
  ):
        Parser[S] =
    new Parser[S] {
      def apply(c: HCursor): Return[S] =
        l(c.downArray)
          .map {
            tail ⇒
              new DataType {
                val size: Int = tail.size
                type T = S
                @inline def apply(buff: ByteBuffer, idx: Int): S =
                  g.from(
                    tail(
                      buff,
                      idx
                    )
                  )
              }
          }
    }

  trait StructParser[L <: HList] {
    def apply(c: ACursor): Return[L]
  }

  object StructParser {
    implicit val hnil:
          StructParser[HNil] =
      new StructParser[HNil] {
        def apply(c: ACursor): Return[HNil] =
          c
            .values match {
            case None ⇒
              Right(
                DataType.hnil
              )
            case Some(v) ⇒
              Left(
                DecodingFailure(
                  s"${v.size} extra elements: $v",
                  c.history
                )
              )
          }
      }

    implicit def cons[H, T <: HList](
      implicit
      head: Lazy[Parser[H]],
      tail: Lazy[StructParser[T]],
      dt: DataType.Aux[H :: T]
    ):
          StructParser[H :: T] =
      new StructParser[H :: T] {
        def apply(c: ACursor): Return[H :: T] =
          for {
            c ←
              c
                .success
                .map(Right(_))
                .getOrElse(
                  Left(
                    DecodingFailure(
                      "Bad starting cursor",
                      c.history
                    )
                  )
                )
            h ← head.value(c)
            t ← tail.value(c.right)
          } yield
            dt
      }
  }
}


object DataType
  extends StructDerivations {

  sealed abstract class Primitive(
    val order: ByteOrder,
    val dType: DType,
    val size: Int
  ) extends DataType {
    override val toString = s"$order$dType$size"
  }

  implicit def dataTypeDecoder[T](implicit parser: Parser[T]): Decoder[Aux[T]] =
    new Decoder[Aux[T]] {
      override def apply(c: HCursor): Result[Aux[T]] = parser(c)
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
  case object   char                                 extends Primitive( None, int,    1) { type T =   Char; @inline def apply(buf: ByteBuffer, idx: Int): T = {                   buf.getChar  (    idx) } }
  case  class    i32(override val order: Endianness) extends Primitive(order, int,    4) { type T =    Int; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getInt   (4 * idx) } }
  case  class    i64(override val order: Endianness) extends Primitive(order, int,    8) { type T =   Long; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getLong  (8 * idx) } }
  case  class  float(override val order: Endianness) extends Primitive(order, flt,    4) { type T =  Float; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getFloat (4 * idx) } }
  case  class double(override val order: Endianness) extends Primitive(order, flt,    8) { type T = Double; @inline def apply(buf: ByteBuffer, idx: Int): T = { buf.order(order); buf.getDouble(8 * idx) } }
  case  class string(override val  size:        Int) extends Primitive( None, str, size) { type T = String
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

object Int {
  def unapply(s: List[Char]): Option[Int] = Some( s.mkString.toInt )
}
