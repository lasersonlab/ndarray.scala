package org.lasersonlab.zarr.dtype

import java.nio.ByteBuffer
import java.nio.ByteBuffer._

import cats.implicits._
import io.circe.DecodingFailure.fromThrowable
import io.circe._
import lasersonlab.xscala._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.|
import shapeless._

import scala.collection.immutable.ListMap
import scala.util.Try

/**
 * Representation of a Zarr record-type; includes functionality for IO to and from a [[ByteBuffer]]
 *
 * Subclasses:
 *
 * - [[Primitive]]s:
 *   - Integer types: [[I8]], [[I16]], [[I32]], [[I64]]
 *   - Floating-point types: [[F32]], [[F64]]
 *   - [[DataType.string string]]
 * - Structs:
 *   - [[struct "typed"]]: auto-derived for a case-class
 *   - [[struct.? "untyped"]]: "bag of fields", corresponding to [[org.lasersonlab.zarr.untyped.Struct]]
 *
 * "Typed" and "Untyped" structs can represent the same logical underlying datatype (e.g. the JSON representation, in
 * [[org.lasersonlab.zarr.Metadata array metadata]]'s "dtype" field will be the same), but allow for call-sites that
 * know/enforce a more structured schema vs. not
 */
sealed trait DataType[_T] extends DataType.? { type T = _T }

object DataType
  extends StructDerivations
     with EqInstances
     with Coders {

  sealed trait ? {
    def size: Int
    type T
    def apply(buff: ByteBuffer): T
    def  read(buff: ByteBuffer, idx: Int): T = {
      buff.position(size * idx)
      apply(buff)
    }
    def apply(buffer: ByteBuffer, t: T): Unit
    def apply(t: T): Array[Byte] = {
      val buff = allocate(size)
      apply(buff, t)
      buff.array()
    }
    def t: DataType[T] = this match { case aux: DataType[T] ⇒ aux }
  }

  type T[T] = DataType[T]

  /**
   * Common interface for non-struct datatypes (numerics, strings)
   */
  sealed abstract class Primitive[T](
    val order: ByteOrder,
    val dType: DType,
    val size: Int
  ) extends DataType[T] {
    override val toString = s"$order$dType$size"
  }

  import ByteOrder._
  type Order = ByteOrder

  import org.lasersonlab.zarr.dtype.{ DType ⇒ d }

  val `0` = 0.toByte

  // TODO: setting the buffer's order every time seems suboptimal, but is necessary as long as structs with fields of
  // opposing endianness are allowed; consider disallowing such structs, and keeping an eye out for this in general
  // profiling (so far it's not been observed to matter)
  case object    I8                                  extends Primitive[  Byte]( None, d.   int,    1) { @inline def apply(buf: ByteBuffer): T = {                   buf.get       }; @inline override def apply(b: ByteBuffer, t: T) = b             .put      (t) }
  case  class    I16(override val order: Endianness) extends Primitive[ Short](order, d.   int,    2) { @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getShort  }; @inline override def apply(b: ByteBuffer, t: T) = b.order(order).putShort (t) }
  case  class    I32(override val order: Endianness) extends Primitive[   Int](order, d.   int,    4) { @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getInt    }; @inline override def apply(b: ByteBuffer, t: T) = b.order(order).putInt   (t) }
  case  class    I64(override val order: Endianness) extends Primitive[  Long](order, d.   int,    8) { @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getLong   }; @inline override def apply(b: ByteBuffer, t: T) = b.order(order).putLong  (t) }
  case  class    F32(override val order: Endianness) extends Primitive[ Float](order, d. float,    4) { @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getFloat  }; @inline override def apply(b: ByteBuffer, t: T) = b.order(order).putFloat (t) }
  case  class    F64(override val order: Endianness) extends Primitive[Double](order, d. float,    8) { @inline def apply(buf: ByteBuffer): T = { buf.order(order); buf.getDouble }; @inline override def apply(b: ByteBuffer, t: T) = b.order(order).putDouble(t) }
  case  class string(override val  size:        Int) extends Primitive[String]( None, d.string, size) {
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

    val maxValue = Byte.MaxValue.toChar

    def apply(buffer: ByteBuffer, t: String): Unit = {
      var i = 0
      while (i < t.length) {
        val ch = t(i)
        if (ch > maxValue)
          throw new IllegalArgumentException(
            s"Invalid character in string $t at position $i: $ch"
          )
        buffer.put(ch.toByte)
        i += 1
      }
      while (i < size) {
        buffer.put(`0`)
        i += 1
      }
    }
  }

  type byte = I8.type

  /**
   * Construct primitive data-types, in the presence of an implicit [[Endianness]], either from an un-applied reference
   * to the companion object (e.g. `float` instead of `float(LittleEndian)`)
   */

  type <>! = Endianness

  /**
   * Expose implicit [[Primitive]] instances for derivations (assuming sufficient [[Endianness]] evidence)
   */

  implicit val   byte                             = I8
  implicit def  short(implicit e: <>!): T[ Short] = I16(e)
  implicit def    int(implicit e: <>!): T[   Int] = I32(e)
  implicit def   long(implicit e: <>!): T[  Long] = I64(e)
  implicit def  float(implicit e: <>!): T[ Float] = F32(e)
  implicit def double(implicit e: <>!): T[Double] = F64(e)

  case class StructEntry(name: String, datatype: DataType.?) {
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

  case class StructList[L <: HList](
    entries: List[StructEntry],
    size: Int
  )(
    read: ByteBuffer ⇒ L,
    write: (ByteBuffer, L) ⇒ Unit
  ) {
    type T = L
    @inline def apply(buff: ByteBuffer): T = read(buff)
    @inline def apply(buffer: ByteBuffer, t: L): Unit = write(buffer, t)
  }

  case class struct[
    S,
    L <: HList
  ](
    entries: StructList[L]
  )(
    implicit
    g: LabelledGeneric.Aux[S, L]
  )
  extends DataType[S] {
    val size: Int = entries.size
    @inline def apply(buffer: ByteBuffer): T = g.from(entries(buffer))
    @inline def apply(buffer: ByteBuffer, t: S): Unit = entries(buffer, g.to(t))
  }

  object struct {
    /**
     * [[zarr.untyped.Struct "Untyped" struct]] [[DataType]]
     */
    case class ?(entries: List[StructEntry])
      extends DataType[zarr.untyped.Struct] {

      val size: Int =
        entries
          .map(_.size)
          .sum

      def apply(buff: ByteBuffer): T =
        zarr.untyped.Struct(
          entries
            .foldLeft(
              ListMap.newBuilder[String, Any]
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

      def apply(buffer: ByteBuffer, t: zarr.untyped.Struct): Unit = {
        val es = entries.iterator
        val fields = t.iterator

        while (es.hasNext) {
          val (StructEntry(name, datatype), (k, v)) = (es.next, fields.next)
          if (k != name)
            throw new IllegalStateException(
              s"Incorrect field in struct: [$k,$v] (expected: [$name,$datatype]). Full struct: ${
                t
                  .map { case (k, v) ⇒ s"[$k,$v]" }
                  .mkString(" ")
              }"
            )

          datatype(buffer, v.asInstanceOf[datatype.T])
        }
        if (fields.hasNext)
          throw new IllegalStateException(
            s"Extra fields found in struct: ${
              t
                .map { case (k, v) ⇒ s"[$k,$v]" }
                .mkString(" ")
            } (expected: ${entries.mkString(" ")})"
          )
      }
    }

    object ? {
      // "magnet pattern" constructor
      case class Arg(entry: StructEntry)
      object Arg {
        implicit def str(t: (String, DataType.?)): Arg = Arg(StructEntry(t._1     , t._2))
        implicit def sym(t: (Symbol, DataType.?)): Arg = Arg(StructEntry(t._1.name, t._2))
        implicit def unwrap(arg: Arg): StructEntry = arg.entry
      }
      def apply(args: Arg*): ? =
        new ?(
          args
            .map(_.entry)
            .toList
        )
    }
  }

  def get(order: ByteOrder, dtype: DType, size: Int): String | DataType.? =
    (order, dtype, size) match {
      case (  None, _: d.   int,    1) ⇒ Right(I8      )
      case (e: <>!, _: d.   int,    2) ⇒ Right(I16(e))
      case (e: <>!, _: d.   int,    4) ⇒ Right(I32(e))
      case (e: <>!, _: d.   int,    8) ⇒ Right(I64(e))
      case (e: <>!, _: d. float,    4) ⇒ Right(F32(e))
      case (e: <>!, _: d. float,    8) ⇒ Right(F64(e))
      case (  None, _: d.string, size) ⇒ Right(string(size))
      case _ ⇒
        Left(
          s"Unrecognized data type: $order$dtype$size"
        )
    }

  val regex = """(.)(.)(\d+)""".r
  def get(str: String, c: HCursor): DecodingFailure | DataType.? =
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
      (order, dtype, size) = t
         order ← ByteOrder.map.get(order).fold[DecodingFailure | ByteOrder] { Left(DecodingFailure(s"Unrecognized order: $order", c.history)) } { Right(_) }
         dtype ←     DType.map.get(dtype).fold[DecodingFailure |     DType] { Left(DecodingFailure(s"Unrecognized dtype: $dtype", c.history)) } { Right(_) }
          size ← Try(size.toInt).toEither.left.map(  fromThrowable(_, c.history))
      datatype ←  get(order, dtype, size).left.map(DecodingFailure(_, c.history))
    } yield
      datatype
}
