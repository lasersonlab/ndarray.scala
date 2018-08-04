package org.lasersonlab.zarr

import java.nio.ByteBuffer

import org.lasersonlab.ndarray.Read
import shapeless._
import shapeless.ops.hlist.Prepend

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
}

sealed abstract class DataType[T](
  order: ByteOrder,
  dType: DType,
  size: Int,
  read: (ByteBuffer, Int) ⇒ T
)
extends Read[T]
{
  override def toString = s"$order$dType$size"
  @inline def apply(buff: ByteBuffer, idx: Int): T =
    read(buff, idx)
}
object DataType {

  import ByteOrder._
  type Order = ByteOrder

  import DType.{
    float ⇒ f,
    string ⇒ s,
    _
  }

  import scala.Array.fill

  case class    i32(order: Endianness) extends DataType[   Int](order, int,    4, (buf, idx) ⇒ { buf.order(order); buf.getInt(4 * idx) })
  case class    i64(order: Endianness) extends DataType[  Long](order, int,    8, (buf, idx) ⇒ { buf.order(order); buf.getLong(8 * idx) })
  case class  float(order: Endianness) extends DataType[Double](order,   f,    4, (buf, idx) ⇒ { buf.order(order); buf.getFloat(4 * idx) })
  case class double(order: Endianness) extends DataType[Double](order,   f,    8, (buf, idx) ⇒ { buf.order(order); buf.getDouble(8 * idx) })
  case class string( size:        Int) extends DataType[String]( None,   s, size, (buf, idx) ⇒ {
    val arr = fill(size)(0.toByte)
    buf.get(arr, size * idx, size)
    arr.map(_.toChar).mkString
  })
}

/**
 * Summon the single element of type [[T]], if it exists (i.e. [[T]] is a case object / has [[Generic]] representation
 * [[HNil]])
 */
trait Singleton[T] {
  def apply(): T
}
object Singleton {
  implicit def singleton[T](implicit g: Generic.Aux[T, HNil]): Singleton[T] =
    new Singleton[T] {
      def apply(): T = g.from(HNil)
    }
}

trait Instances[In] {
  type Out <: HList
  def apply(): Out
}
trait LowPriInstances {
  type Aux[In, _O <: HList] = Instances[In] { type Out = _O }
  def apply[In, _O <: HList](out: _O): Aux[In, _O] =
    new Instances[In] {
      type Out = _O
      def apply(): Out = out
    }
  implicit def product[
    T,
    L <: HList
  ](
    implicit
    g: Generic.Aux[T, L],
    i: Lazy[Instances[L]]
  ):
    Aux[
      T,
      i.value.Out
    ] =
    apply(
      i.value()
    )

  implicit def cons[
    H,
    L <: HList,
    HI <: HList,
    LI <: HList
  ](
    implicit
    hi: Aux[H, HI],
    li: Aux[L, LI],
    c: Cartesian[HI, LI]
  ):
    Aux[H :: L, c.Out] =
    apply(
      c(
        hi(),
        li()
      )
    )
}
object Instances
  extends LowPriInstances {

  implicit def singleton[T](implicit s: Singleton[T]): Aux[T, T :: HNil] = apply(s() :: HNil)

  implicit val cnil: Aux[CNil, HNil] = apply(HNil)
  implicit val hnil: Aux[HNil, HNil] = apply(HNil)

  implicit def one[T](
    implicit
    i: Instances[T]
  ):
    Aux[
      T :: HNil,
      i.Out
    ] =
    apply(
      i()
    )

  implicit def ccons[
    H,
    C <: Coproduct,
    HI <: HList,
    CI <: HList
  ](
    implicit
    hi: Lazy[Aux[H, HI]],
    ci: Lazy[Aux[C, CI]],
    pp: Prepend[HI, CI]
  ):
    Aux[
      H :+: C,
      pp.Out
    ] =
    apply(
      pp(
        hi.value(),
        ci.value()
      )
    )

  implicit def coproduct[
    T,
    C <: Coproduct
  ](
    implicit
    g: Generic.Aux[T, C],
    i: Lazy[Instances[C]]
  ):
    Aux[
      T,
      i.value.Out
    ] =
    apply(
      i.value()
    )
}

/**
 * Compute an [[HList]] comprised of all the options in a [[Coproduct]]
 */
trait Options[In] {
  type Out <: HList
}
object Options {
  type Aux[In, _O <: HList] = Options[In] { type Out = _O }
  implicit val nil: Aux[CNil, HNil] =
    new Options[CNil] {
      type Out = HNil
    }

  implicit def cons[H, C <: Coproduct](implicit o: Options[C]): Aux[H :+: C, H :: o.Out] =
    new Options[H :+: C] {
      type Out = H :: o.Out
    }

  implicit def coproduct[T, C <: Coproduct](implicit g: Generic.Aux[T, C], o: Options[C]): Aux[T, o.Out] =
    new Options[T] {
      type Out = o.Out
    }
}

/**
 * Prepend an [[H element]] to [[T another]]; if the "tail" is already an [[HList]], the usual [[:: prepend]] is
 * performed; otherwise, a two-element [[HList]] is constructed from the two arguments
 */
trait ElementPrepend[H, T] {
  type Out <: HList
  def apply(h: H, t: T): Out
}
trait LowPriElementPrepend {
  type Aux[H, T, _O <: HList] = ElementPrepend[H, T] { type Out = _O }
  implicit def fallback[H, T]: Aux[H, T, H :: T:: HNil] =
    new ElementPrepend[H, T] {
      type Out = H :: T :: HNil
      def apply(h: H, t: T): Out = h :: t :: HNil
    }
}
object ElementPrepend
  extends LowPriElementPrepend {
  implicit def hlist[H, T <: HList]: Aux[H, T, H :: T] =
    new ElementPrepend[H, T] {
      type Out = H :: T
      def apply(h: H, t: T): Out = h :: t
    }
}

/**
 * Prepend an [[E element]] to each element in an [[L HList]]
 */
trait ElemwisePrepend[E, L <: HList] {
  type Out <: HList
  def apply(e: E, l: L): Out
}
object ElemwisePrepend {
  type Aux[E, L <: HList, _O] = ElemwisePrepend[E, L] { type Out = _O }
  implicit def hnil[T]: Aux[T, HNil, HNil] =
    new ElemwisePrepend[T, HNil] {
      type Out = HNil
      def apply(e: T, l: HNil): Out = HNil
    }

  implicit def cons[
    E,
    H,
    L <: HList
  ](
    implicit
    ep: ElemwisePrepend[E, L],
    el: ElementPrepend[E, H]
  ):
    Aux[
      E,
      H :: L,
      el.Out :: ep.Out
    ] =
    new ElemwisePrepend[E, H :: L] {
      type Out = el.Out :: ep.Out
      def apply(e: E, l: H :: L): Out =
        l match {
          case h :: t ⇒
            el(e, h) :: ep(e, t)
        }
    }
}

/**
 * Cartesian product of two [[HList]]s
 */
trait Cartesian[L <: HList, R <: HList] {
  type Out <: HList
  def apply(l: L, r: R): Out
}
trait LowPriCartesian {
  type Aux[L <: HList, R <: HList, _Out <: HList] = Cartesian[L, R] { type Out = _Out }
  def apply[L <: HList, R <: HList, _Out <: HList](fn: (L, R) ⇒ _Out): Aux[L, R, _Out] =
    new Cartesian[L, R] {
      type Out = _Out
      @inline def apply(l: L, r: R): Out = fn(l, r)
    }

  implicit def rhnil[L <: HList]: Aux[L, HNil, HNil] = apply((_, _) ⇒ HNil)
}
trait LowPriCartesian2
  extends LowPriCartesian {
  implicit def lhnil[R <: HList]: Aux[HNil, R, HNil] = apply((_, _) ⇒ HNil)
}
trait LowPriCartesian3
  extends LowPriCartesian2 {
  implicit def lbase[
    L,
    R <: HList
  ](
    implicit
    ep: ElemwisePrepend[L, R]
  ):
    Aux[
      L :: HNil,
      R,
      ep.Out
    ] =
    apply {
      case (
        l :: HNil,
        r
      ) ⇒
        ep(l, r)
    }
}
object Cartesian
  extends LowPriCartesian3 {
  implicit def lcons[
       H,
       L <: HList,
       R <: HList,
    Prev <: HList,
     New <: HList
  ](
    implicit
    c: Aux[L, R, Prev],
    e: ElemwisePrepend.Aux[H, R, New],
    p: Prepend[New, Prev]
  ):
    Aux[
      H :: L,
      R,
      p.Out
    ] =
    apply {
      case (h :: l, r) ⇒
        p(
          e(h, r),
          c(l, r)
        )
    }
}
