package org.lasersonlab.zarr.dtype

import java.nio.ByteBuffer

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.lasersonlab.ndarray.io.Read
import org.lasersonlab.zarr.|

import scala.util.Try

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

  case class StructEntry[T](override val toString: String, size: Int)
  case class Struct[S](entries: List[StructEntry[_]])(fn: ByteBuffer ⇒ S) extends DataType {
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
    Struct(l.value.entries) {
      buff ⇒
        g.from(
          l.value(buff)
        )
    }
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
