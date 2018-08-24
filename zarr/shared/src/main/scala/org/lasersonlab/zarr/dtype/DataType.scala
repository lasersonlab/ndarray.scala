package org.lasersonlab.zarr.dtype

import java.nio.ByteBuffer

import cats.Eq
import io.circe.Decoder.Result
import io.circe.DecodingFailure.fromThrowable
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.lasersonlab.ndarray.io.Read
import org.lasersonlab.zarr
import org.lasersonlab.zarr.{ dtype, untyped, | }
import shapeless.ops.hlist.LiftAll
import shapeless.{ :+:, CNil, Generic, HList, the }

import scala.util.Try

/**
 * Representation of a Zarr record-type; includes functionality for reading a record from a [[ByteBuffer]]
 */
sealed trait DataType {
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

  case class StructEntry[T](name: String, datatype: Aux[T]) {
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
    L <: HList,
    D <: HList
  ](
    entries: StructList[L, D]
  )(
    implicit
    g: Generic.Aux[S, L]
  )
  extends DataType {
    val size: Int = entries.size
    override type T = S
    @inline def apply(buff: ByteBuffer): T = g.from(entries(buff))
  }

  case class StructList[
    L <: HList,
    D <: HList
  ](
    entries: D,
    size: Int
  )(
    fn: ByteBuffer ⇒ L
  )(
    implicit
    val ev: LiftAll.Aux[StructEntry, L, D]
  )
  extends DataType {
    type T = L
    @inline def apply(buff: ByteBuffer): T = fn(buff)
  }

  implicit val hnil: StructList[HNil, HNil] =
    StructList[
      HNil,
      HNil
    ](
      entries = HNil,
      size = 0
    )(
      _ ⇒ HNil
    )

  implicit def cons[
    Head,
    Tail <: HList,
    DTail <: HList
  ](
    implicit
    head: Aux[Head],
    tail: StructList[Tail, DTail],
  ):
    StructList[
      Head :: Tail,
      StructEntry[Head] :: DTail
    ]
  = {
    implicit val headEntry =
      StructEntry(
        head.toString,
        head
      )

    import tail._

    StructList(
      headEntry :: tail.entries,
      head.size  + tail.size
    )(
      buff ⇒
        head(buff) ::
        tail(buff)
    )
  }

  implicit def _struct[
    S,
    L <: HList,
    D <: HList
  ](
    implicit
    g: Generic.Aux[S, L],
    l: StructList[L, D]
  ):
    Aux[S] =
    struct[S, L, D](g, l)

  def struct[
    S,
    L <: HList,
    D <: HList
  ](
    implicit
    g: Generic.Aux[S, L],
    entries: StructList[L, D]
  ):
    Aux[S] =
    Struct(entries)
}

object DataType
  extends DataTypeStructDerivations {

//  implicit val generic:
//    Generic.Aux[
//      DataType,
//      byte :+:
//      short :+:
//      int :+:
//      long :+:
//      float :+:
//      double :+:
//      string :+:
//      struct :+:
//      Struct[_, _, _] :+:
//      StructList[_, _] :+:
//      CNil
//    ] =
//    new Generic[DataType] {
//      type Repr =
//        byte :+:
//        short :+:
//        int :+:
//        long :+:
//        float :+:
//        double :+:
//        string :+:
//        struct :+:
//        Struct[_, _, _] :+:
//        StructList[_, _] :+:
//        CNil
//
//      def to(t: DataType): Repr = ???
//      def from(r: Repr): DataType = ???
//    }

  lazy val structEq: Eq[struct] =
    Eq.by[
      struct,
      List[
        (
          String,
          DataType
        )
      ]
    ](
      _
        .entries
        .toList
    )(
      structEntriesEq
    )

  val structEntriesEq: Eq[List[(String, DataType)]] =
    new Eq[List[(String, DataType)]] {
      def eqv(x: List[(String, DataType)], y: List[(String, DataType)]): Boolean =
        (x, y) match {
          case (Nil, Nil) ⇒ true
          case ((nx, dx) :: tx, (ny, dy) :: ty) ⇒
            nx == ny &&
            dataTypeEq.eqv(dx, dy) &&
            eqv(tx, ty)
          case _ ⇒ false
        }
    }

  implicit val dataTypeEq: Eq[DataType] =
    new Eq[DataType] {
      import cats.implicits.catsKernelStdOrderForInt
      import cats.derived.auto.eq._
      the[Eq[byte.type]]
      the[Eq[int]]
      the[Eq[short]]
      the[Eq[long]]
      the[Eq[float]]
      the[Eq[double]]
      the[Eq[string]]
      val primitive = the[Eq[Primitive]]
      val hlistEq: Eq[HList] = ??? //the[Eq[HList]]

      //val Struct = the[Eq[DataType.Struct[_, _, _]]]
      def eqv(x: DataType, y: DataType): Boolean =
        (x, y) match {
          case (x: Primitive, y: Primitive) ⇒ primitive.eqv(x, y)
          case (x: struct, y: struct) ⇒ structEq.eqv(x, y)
          case (Struct(StructList(xEntries, xSize)), Struct(StructList(yEntries, ySize))) ⇒
            hlistEq.eqv(xEntries, yEntries) &&
            x.size == y.size
          case (StructList(xEntries, xSize), StructList(yEntries, ySize)) ⇒
            hlistEq.eqv(xEntries, yEntries) &&
              x.size == y.size
          case _ ⇒ false
        }
    }

//  val StructEq: Eq[Struct[_, _, _]] = Eq.by[Struct[_, _, _], StructList[_, _]](_.entries)(StructListEq)
//
//  val StructListEq: Eq[StructList[_, _]] =
//    new Eq[StructList[_, _]] {
//      val hlistEq = the[Eq[HList]]
//      def eqv(x: StructList[_, _], y: StructList[_, _]): Boolean =
//        hlistEq.eqv(x.entries, y.entries) &&
//        x.size == y.size
//    }

//  implicit def genericAux[T]:
//    Generic.Aux[
//      Aux[T],
//      byte :+:
//        short :+:
//        int :+:
//        long :+:
//        float :+:
//        double :+:
//        string :+:
//        CNil
//      ] =
//    new Generic[Aux[T]] {
//      type Repr =
//        byte :+:
//          short :+:
//          int :+:
//          long :+:
//          float :+:
//          double :+:
//          string :+:
//          CNil
//
//      def to(t: Aux[T]): Repr = ???
//
//      def from(r: Repr): Aux[T] = ???
//    }


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

  import org.lasersonlab.zarr.dtype.{ DType ⇒ d }

  val `0` = 0.toByte

  // TODO: setting the buffer's order every time seems suboptimal; some different design that streamlines that would be nice
  case object   byte                                 extends Primitive( None, d.   int,    1) { type T =   Byte; @inline def apply(buf: ByteBuffer): T = {                   buf.get       } }
  case  class  short(override val order: Endianness) extends Primitive(order, d.   int,    2) { type T =  Short; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getShort  } }
  case  class    int(override val order: Endianness) extends Primitive(order, d.   int,    4) { type T =    Int; @inline def apply(buf: ByteBuffer): T = {buf.order(order); buf.getInt    } }
  case  class   long(override val order: Endianness) extends Primitive(order, d.   int,    8) { type T =   Long; @inline def apply(buf: ByteBuffer): T = {buf.order(order); buf.getLong   } }
  case  class  float(override val order: Endianness) extends Primitive(order, d. float,    4) { type T =  Float; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getFloat  } }
  case  class double(override val order: Endianness) extends Primitive(order, d. float,    8) { type T = Double; @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getDouble } }
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
  }

  type byte = byte.type

  object  short { implicit def apply(v:  short.type)(implicit endianness: Endianness): Aux[ Short] =  short(endianness) }
  object    int { implicit def apply(v:    int.type)(implicit endianness: Endianness): Aux[   Int] =    int(endianness) }
  object   long { implicit def apply(v:   long.type)(implicit endianness: Endianness): Aux[  Long] =   long(endianness) }
  object  float { implicit def apply(v:  float.type)(implicit endianness: Endianness): Aux[ Float] =  float(endianness) }
  object double { implicit def apply(v: double.type)(implicit endianness: Endianness): Aux[Double] = double(endianness) }

  case class struct(entries: Seq[(String, DataType)])
    extends DataType {
    type T = untyped.Struct

    val size: Int =
      entries
        .map(_._2.size)
        .sum

    def apply(buff: ByteBuffer): T =
      untyped.Struct(
        entries
          .foldLeft(
            Map.newBuilder[String, Any]
          ) {
            case (
              builder,
              (
                name,
                datatype
              )
            ) ⇒
              builder +=
                name → datatype(buff)
          }
          .result()
      )
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
