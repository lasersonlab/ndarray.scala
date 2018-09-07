package org.lasersonlab.zarr.dtype

import java.nio.ByteBuffer
import java.nio.ByteBuffer._

import cats.Eq
import io.circe.Decoder.Result
import io.circe.DecodingFailure.fromThrowable
import io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json }
import org.lasersonlab.ndarray.io.{ Read, Write }
import org.lasersonlab.zarr.{ untyped, | }
import shapeless.the

import scala.util.Try

/**
 * Representation of a Zarr record-type; includes functionality for reading a record from a [[ByteBuffer]]
 */
sealed trait DataType {
  def size: Int
  type T
  def apply(buff: ByteBuffer): T
  def read(buff: ByteBuffer, idx: Int): T = {
    buff.position(size * idx)
    apply(buff)
  }
  def apply(buffer: ByteBuffer, t: T): Unit
  def apply(t: T): Array[Byte] = {
    val buff = allocate(size)
    apply(buff, t)
    buff.array()
  }
}

trait DataTypeStructDerivations {
  import shapeless._

  type Aux[_T] = DataType { type T = _T }

  case class StructEntry(name: String, datatype: DataType) {
    val size = datatype.size
    override def toString: String =
      Seq(
        name,
        size
      )
      .mkString(
        "[\"",
        "\",\"",
        "\"]"
      )
  }

  // TODO: make the parsing just use the entries?
  case class Struct[
    S,
    L <: HList
  ](
    entries: StructList[L]
  )(
    implicit
    g: Generic.Aux[S, L]
  )
  extends DataType {
    val size: Int = entries.size
    override type T = S
    @inline def apply(buff: ByteBuffer): T = g.from(entries(buff))
    @inline def apply(buffer: ByteBuffer, t: S): Unit = entries(buffer, g.to(t))
  }

  case class StructList[L <: HList](
    entries: List[StructEntry],
    size: Int
  )(
    read: ByteBuffer ⇒ L,
    write: (ByteBuffer, L) ⇒ Unit
  )
  extends DataType {
    type T = L
    @inline def apply(buff: ByteBuffer): T = read(buff)
    @inline def apply(buffer: ByteBuffer, t: L): Unit = write(buffer, t)
  }

  implicit val hnil: StructList[HNil] =
    StructList[HNil](
      entries = Nil,
      size = 0
    )(
      _ ⇒ HNil,
      (_, _) ⇒ ()
    )

  implicit def cons[
    Head,
    Tail <: HList
  ](
    implicit
    head: Aux[Head],
    tail: StructList[Tail],
  ):
    StructList[Head :: Tail]
  = {
    implicit val headEntry =
      StructEntry(
        head.toString,
        head
      )

    StructList(
      headEntry :: tail.entries,
      head.size  + tail.size
    )(
      buff ⇒
        head(buff) ::
        tail(buff),
      {
        case (buffer, h :: t) ⇒
          head(buffer, h)
          tail(buffer, t)
      }
    )
  }

  implicit def _struct[
    S,
    L <: HList
  ](
    implicit
    g: Generic.Aux[S, L],
    l: StructList[L]
  ):
    Aux[S] =
    struct[S, L](g, l)

  def struct[
    S,
    L <: HList
  ](
    implicit
    g: Generic.Aux[S, L],
    entries: StructList[L]
  ):
    Aux[S] =
    Struct(entries)
}

object DataType
  extends DataTypeStructDerivations {

  lazy val structEq: Eq[struct] =
    Eq.by[
      struct,
      List[StructEntry]
    ](
      _
        .entries
        .toList
    )(
      structEntriesEq
    )

  lazy val structEntriesEq: Eq[List[StructEntry]] =
    new Eq[List[StructEntry]] {
      def eqv(
        x: List[StructEntry],
        y: List[StructEntry]
      ):
        Boolean =
        (x, y) match {
          case (Nil, Nil) ⇒ true
          case (
            StructEntry(nx, dx) :: tx,
            StructEntry(ny, dy) :: ty
          ) ⇒
            nx == ny &&
            dataTypeEq.eqv(dx, dy) &&
            eqv(tx, ty)
          case _ ⇒ false
        }
    }

  implicit lazy val dataTypeEq: Eq[DataType] =
    new Eq[DataType] {
      import cats.derived.auto.eq._
      import cats.implicits.catsKernelStdOrderForInt
      val primitive = the[Eq[Primitive]]

      def eqv(x: DataType, y: DataType): Boolean =
        (x, y) match {
          case (x: Primitive, y: Primitive) ⇒ primitive.eqv(x, y)
          case (x: struct, y: struct) ⇒ structEq.eqv(x, y)
          case (Struct(StructList(xEntries, xSize)), Struct(StructList(yEntries, ySize))) ⇒
            structEntriesEq.eqv(xEntries, yEntries) &&
            x.size == y.size
          case (StructList(xEntries, xSize), StructList(yEntries, ySize)) ⇒
            structEntriesEq.eqv(xEntries, yEntries) &&
              x.size == y.size
          case _ ⇒ false
        }
    }

  sealed abstract class Primitive(
    val order: ByteOrder,
    val dType: DType,
    val size: Int
  ) extends DataType {
    override val toString = s"$order$dType$size"
  }

  implicit def dataTypeDecoder[T](implicit parser: Parser[T]): Decoder[Aux[T]] =
    new Decoder[Aux[T]] {
      def apply(c: HCursor): Result[Aux[T]] = parser(c)
    }

  implicit def dataTypeEncoder[D <: DataType]: Encoder[D] =
    new Encoder[D] {
      def apply(entries: Seq[StructEntry]): Json =
        Json.arr(
          entries
            .map {
              case StructEntry(name, datatype) ⇒
                Json.arr(
                  Json.fromString(name),
                  dataTypeEncoder(datatype)
                )
            }:
            _*
        )
      def apply(a: D): Json =
        a match {
          case p: Primitive ⇒ Json.fromString(p.toString)
          case        StructList(entries, _)  ⇒ apply(entries)
          case Struct(StructList(entries, _)) ⇒ apply(entries)
          case struct           (entries   )  ⇒ apply(entries)
        }
    }

  implicit def read[T](implicit dataType: DataType.Aux[T]): Read[T] =
    new Read[T] {
      @inline def apply(buff: ByteBuffer, idx: Int): T = dataType.read(buff, idx)
    }

  implicit def write[T](implicit dataType: DataType.Aux[T]): Write[T] =
    new Write[T] {
      @inline def apply(buffer: ByteBuffer, t: T): Unit = dataType(buffer, t)
    }

  import ByteOrder._
  type Order = ByteOrder

  import org.lasersonlab.zarr.dtype.{ DType ⇒ d }

  val `0` = 0.toByte

  // TODO: setting the buffer's order every time seems suboptimal; some different design that streamlines that would be nice
  case object   byte                                 extends Primitive( None, d.   int,    1) { type T =   Byte; @inline def apply(buf: ByteBuffer): T = {                   buf.get       }; @inline override def apply(b: ByteBuffer, t: T) = b             .put      (t) }
  case  class  short(override val order: Endianness) extends Primitive(order, d.   int,    2) { type T =  Short; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getShort  }; @inline override def apply(b: ByteBuffer, t: T) = b.order(order).putShort (t) }
  case  class    int(override val order: Endianness) extends Primitive(order, d.   int,    4) { type T =    Int; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getInt    }; @inline override def apply(b: ByteBuffer, t: T) = b.order(order).putInt   (t) }
  case  class   long(override val order: Endianness) extends Primitive(order, d.   int,    8) { type T =   Long; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getLong   }; @inline override def apply(b: ByteBuffer, t: T) = b.order(order).putLong  (t) }
  case  class  float(override val order: Endianness) extends Primitive(order, d. float,    4) { type T =  Float; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getFloat  }; @inline override def apply(b: ByteBuffer, t: T) = b.order(order).putFloat (t) }
  case  class double(override val order: Endianness) extends Primitive(order, d. float,    8) { type T = Double; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getDouble }; @inline override def apply(b: ByteBuffer, t: T) = b.order(order).putDouble(t) }
  case  class string(override val  size:        Int) extends Primitive( None, d.string, size) { type T = String
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

//    override def apply(t: String): Array[Byte] = {
//      val arr = new Array[Byte](t.length)
//      var i = 0
//      if (t.length != size)
//        throw new IllegalArgumentException(
//          s"Expected string of size $size, found ${t.length}: '$t'"
//        )
//      while (i < t.length) {
//        val ch = t(i)
//        if (!ch.isValidByte)
//          throw new IllegalArgumentException(
//            s"Invalid character in string $t at position $i: $ch"
//          )
//        arr(i) = ch.toByte
//        i += 1
//      }
//      arr
//    }

    override def apply(buffer: ByteBuffer, t: String): Unit = {
      var i = 0
      if (t.length != size)
        throw new IllegalArgumentException(
          s"Expected string of size $size, found ${t.length}: '$t'"
        )
      while (i < t.length) {
        val ch = t(i)
        if (!ch.isValidByte)
          throw new IllegalArgumentException(
            s"Invalid character in string $t at position $i: $ch"
          )
        buffer.put(ch.toByte)
        i += 1
      }
    }
  }

  type byte = byte.type

  object  short { implicit def apply(v:  short.type)(implicit endianness: Endianness): Aux[ Short] =  short(endianness) }
  object    int { implicit def apply(v:    int.type)(implicit endianness: Endianness): Aux[   Int] =    int(endianness) }
  object   long { implicit def apply(v:   long.type)(implicit endianness: Endianness): Aux[  Long] =   long(endianness) }
  object  float { implicit def apply(v:  float.type)(implicit endianness: Endianness): Aux[ Float] =  float(endianness) }
  object double { implicit def apply(v: double.type)(implicit endianness: Endianness): Aux[Double] = double(endianness) }

  case class struct(entries: Seq[StructEntry])
    extends DataType {
    type T = untyped.Struct

    val size: Int =
      entries
        .map(_.size)
        .sum

    def apply(buff: ByteBuffer): T =
      untyped.Struct(
        entries
          .foldLeft(
            Map.newBuilder[String, Any]
          ) {
            case (
              builder,
              StructEntry(
                name,
                datatype
              )
            ) ⇒
              builder +=
                name → datatype(buff)
          }
          .result()
      )

    override def apply(buffer: ByteBuffer, t: untyped.Struct): Unit =
      for {
        StructEntry(name, datatype) ← entries
      } {
        datatype(buffer, t[datatype.T](name))
      }
  }

  implicit val   _byte                                               =   byte
  implicit def  _short(implicit endianness: Endianness): Aux[ Short] =  short(endianness)
  implicit def    _int(implicit endianness: Endianness): Aux[   Int] =    int(endianness)
  implicit def   _long(implicit endianness: Endianness): Aux[  Long] =   long(endianness)
  implicit def  _float(implicit endianness: Endianness): Aux[ Float] =  float(endianness)
  implicit def _double(implicit endianness: Endianness): Aux[Double] = double(endianness)

  def get(order: ByteOrder, dtype: DType, size: Int): String | DataType =
    (order, dtype, size) match {
      case (         None, _: d.   int,    1) ⇒ Right(  byte      )
      case (e: Endianness, _: d.   int,    2) ⇒ Right( short(   e))
      case (e: Endianness, _: d.   int,    4) ⇒ Right(   int(   e))
      case (e: Endianness, _: d.   int,    8) ⇒ Right(  long(   e))
      case (e: Endianness, _: d. float,    4) ⇒ Right( float(   e))
      case (e: Endianness, _: d. float,    8) ⇒ Right(double(   e))
      case (         None, _: d.string, size) ⇒ Right(string(size))
      case _ ⇒
        Left(
          s"Unrecognized data type: $order$dtype$size"
        )
    }

  def get(str: String, c: HCursor): DecodingFailure | DataType =
    for {
      t ←
        Try {
          val regex(order, tpe, size) = str
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

  val regex = """(.)(.)(\d+)""".r
  implicit val decoder: Decoder[DataType] =
    new Decoder[DataType] {
      import cats.implicits._
      def apply(c: HCursor): Result[DataType] =
        c
          .value
          .as[String]
          .fold(
            _ ⇒ Parser.untypedStruct(c),
            get(_, c)
          )
    }
}
