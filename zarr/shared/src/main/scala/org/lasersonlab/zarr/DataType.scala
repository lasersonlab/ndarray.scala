package org.lasersonlab.zarr

import java.nio.ByteBuffer

import io.circe.Decoder.Result
import io.circe.{ ACursor, Decoder, DecodingFailure, HCursor, Json }
import io.circe.parser._
import org.hammerlab.lines.Name
import org.lasersonlab.ndarray.io.Read
import org.lasersonlab.zarr.ByteOrder.Endianness
import org.lasersonlab.zarr.Parser.Return

import scala.PartialFunction.condOpt
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

  type    int =    int.type
  type   bool =   bool.type
  type  float =  float.type
  type string = string.type

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
  def apply(buff: ByteBuffer): T
  def apply(buff: ByteBuffer, idx: Int): T = {
    buff.position(size * idx)
    apply(buff)
  }
}

trait DataTypeStructDerivations {
  import shapeless._

  type Aux[_T] = DataType { type T = _T }

  case class StructEntry[T](override val toString: String, size: Int)//name: String, `type`: Aux[T])
  case class Struct[S](entries: List[StructEntry[_]])(fn: ByteBuffer ⇒ S) extends DataType {
    //val size: Int = entries.map(_.`type`.size).sum
    val size: Int = entries.map(_.size).sum
    override type T = S
    @inline def apply(buff: ByteBuffer): T = fn(buff)
  }

  implicit val hnil: Struct[HNil] = Struct(Nil)(_ ⇒ HNil)

  implicit def cons[
    Head,
    Tail <: HList
  ](
    implicit
    head: Aux[Head],
    tail: Lazy[Struct[Tail]]
  ): Struct[Head :: Tail] =
    Struct(
      scala.::(
        StructEntry(
          head.toString,
          head.size
        ),
        tail.value.entries
      )
    ) {
      buff ⇒
        val h = head(buff)
        val t = tail.value(buff)
        h :: t
    }
//    new DataType {
//      type T = Head :: Tail
//      val size = head.size + tail.value.size
//      def apply(buff: ByteBuffer): Head :: Tail = {
//        val h = head(buff)
//        val t = tail.value(buff)
//        h :: t
//      }
//    }

  implicit def _struct[
     S,
     L <: HList
  ](
    implicit
    g: Generic.Aux[S, L],
    l: Lazy[Struct[L]]
  ):
    Aux[S] =
    struct[S, L]

  def struct[
     S,
     L <: HList
  ](
    implicit
    g: Generic.Aux[S, L],
    l: Lazy[Struct[L]]
  ):
    Struct[S] =
    Struct(l.value.entries) { buff ⇒ g.from(l.value(buff)) }
//    new DataType {
//      val size = l.value.size
//      type T = S
//      def apply(buff: ByteBuffer): S = g.from(l.value(buff))
//    }
}

object DataType
  extends DataTypeStructDerivations {

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
  case object   char                                 extends Primitive( None, int,    1) { type T =   Char; @inline def apply(buf: ByteBuffer): T = {                   buf.getChar   } }
  case  class  short(override val order: Endianness) extends Primitive(order, int,    2) { type T =  Short; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getShort  } }
  case  class    i32(override val order: Endianness) extends Primitive(order, int,    4) { type T =    Int; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getInt    } }
  case  class    i64(override val order: Endianness) extends Primitive(order, int,    8) { type T =   Long; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getLong   } }
  case  class  float(override val order: Endianness) extends Primitive(order, flt,    4) { type T =  Float; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getFloat  } }
  case  class double(override val order: Endianness) extends Primitive(order, flt,    8) { type T = Double; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getDouble } }
  case  class string(override val  size:        Int) extends Primitive( None, str, size) { type T = String
    import scala.Array.fill
    val arr = fill(size)(`0`)
    def apply(buf: ByteBuffer): T = {
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
      case (e: Endianness, _: int,    4) ⇒ Right(   i32(   e))
      case (e: Endianness, _: int,    8) ⇒ Right(   i64(   e))
      case (e: Endianness, _: int,    1) ⇒ Right(  char      )
      case (e: Endianness, _: flt,    4) ⇒ Right( float(   e))
      case (e: Endianness, _: flt,    8) ⇒ Right(double(   e))
      case (         None, _: str, size) ⇒ Right(string(size))
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
                        s"Unrecognized $name dtype: ${str.mkString}",
                        c.history
                      )
                    )
                )
          }
    }

  import DataType._

  implicit val   _char: Parser[  Char] = make { case           '|' :: 'i' :: Int(   1) ⇒   char       }
  implicit val  _short: Parser[ Short] = make { case Endianness(e) :: 'i' :: Int(   2) ⇒  short(   e) }
  implicit val     int: Parser[   Int] = make { case Endianness(e) :: 'i' :: Int(   4) ⇒    i32(   e) }
  implicit val    long: Parser[  Long] = make { case Endianness(e) :: 'i' :: Int(   8) ⇒    i64(   e) }
  implicit val  _float: Parser[ Float] = make { case Endianness(e) :: 'f' :: Int(   4) ⇒  float(   e) }
  implicit val _double: Parser[Double] = make { case Endianness(e) :: 'f' :: Int(   8) ⇒ double(   e) }
  implicit val _string: Parser[String] = make { case           '|' :: 'S' :: Int(size) ⇒ string(size) }

  import shapeless._

  case class StructEntry(name: String, `type`: String)
  object StructEntry {
    implicit val decoder: Decoder[StructEntry] =
      new Decoder[StructEntry] {
        def apply(c: HCursor): Result[StructEntry] =
          c
            .as[Vector[String]]
            .flatMap {
              case Vector(name, tpe) ⇒
                Right(
                  StructEntry(
                    name,
                    tpe
                  )
                )
              case v ⇒
                Left(
                  DecodingFailure(
                    s"Array has unexpected number of elements (${v.length}): ${v.mkString(",")}",
                    Nil
                  )
                )
            }
      }
  }

  implicit def structParser[S, L <: HList](
    implicit
    g: Generic.Aux[S, L],
    l: StructParser[L]
  ):
        Parser[S] =
    new Parser[S] {
      def apply(c: HCursor): Return[S] =
        c
          .as[Vector[StructEntry]]
          .map {
            _.map { _.`type` }
          }
          .flatMap {
            arr ⇒
              l(arr.toList)
                .map(
                  l ⇒
                    DataType.struct[S, L](g, Lazy(l))
                )
          }
    }
}

import shapeless._

trait StructParser[L <: HList] {
  import StructParser.Return
  def apply(c: List[String]): Return[L]
}

object StructParser {
  import DataType.Struct
  type Return[T] = DecodingFailure | Struct[T]

  implicit val hnil:
        StructParser[HNil] =
    new StructParser[HNil] {
      def apply(c: List[String]): Return[HNil] =
        c match {
          case Nil ⇒ Right(DataType.hnil)
          case l ⇒
            Left(
              DecodingFailure(
                s"${l.size} extra elements: ${l.mkString(",")}",
                Nil
              )
            )
        }
    }

  implicit def cons[
    Head,
    Tail <: HList
  ](
    implicit
    head: Lazy[Parser[Head]],
    tail: Lazy[StructParser[Tail]]
  ):
        StructParser[Head :: Tail] =
    new StructParser[Head :: Tail] {
      def apply(c: List[String]): Return[Head :: Tail] =
        c match {
          case Nil ⇒
            Left(
              DecodingFailure(
                "Ran out of elements",
                Nil
              )
            )
          case scala.::(hStr, t) ⇒
            for {
              h ← head.value(HCursor.fromJson(Json.fromString(hStr)))
              t ← tail.value(t)
            } yield
              //Struct()
              DataType.cons[Head, Tail](h, Lazy(t))
        }
    }
}
