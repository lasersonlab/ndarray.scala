package org.lasersonlab.zarr.dtype

import cats.Eq
import org.lasersonlab.zarr.dtype.DataType._
import shapeless.the

/**
 * Instances for comparing two [[DataType]]s, whether they are known to have the same underlying type or not
 */
trait EqInstances {

  //implicit val checkStructTypedness = StructTypednessMatch.yes

  lazy val structEq: Eq[untyped.Struct] =
    Eq.by[
      untyped.Struct,
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
            scala.::(StructEntry(nx, dx), tx),
            scala.::(StructEntry(ny, dy), ty)
          ) ⇒
            nx == ny &&
            dataTypeEq.eqv(dx, dy) &&
            eqv(tx, ty)
          case _ ⇒ false
        }
    }

  implicit def auxEq[T](implicit structTypednessMatch: StructTypednessMatch): Eq[Aux[T]] =
    new Eq[Aux[T]] {
      def eqv(x: Aux[T], y: Aux[T]): Boolean = dataTypeEq(structTypednessMatch).eqv(x, y)
    }

  object StructTypedness {
    implicit val ignore: StructTypednessMatch = StructTypednessMatch.no
  }

  implicit def dataTypeEq(implicit structTypednessMatch: StructTypednessMatch): Eq[DataType] =
    new Eq[DataType] {
      import cats.derived.auto.eq._
      import cats.implicits.catsKernelStdOrderForInt
      val primitive = the[Eq[Primitive[_]]]

      def eqv(x: DataType, y: DataType): Boolean =
        (x, y) match {
          case (x: Primitive[_], y: Primitive[_]) ⇒ primitive.eqv(x, y)
          case (x: untyped.Struct, y: untyped.Struct) ⇒ structEq.eqv(x, y)
          case (
            Struct(
              StructList(
                xEntries,
                xSize
              )
            ),
            Struct(
              StructList(
                yEntries,
                ySize
              )
            )
          ) ⇒
            structEntriesEq.eqv(xEntries, yEntries) &&
            x.size == y.size
          case (untyped.Struct(xEntries), Struct(StructList(yEntries, _)))
            if structTypednessMatch == StructTypednessMatch.no ⇒
            structEntriesEq.eqv(xEntries, yEntries)
          case (Struct(StructList(xEntries, _)), untyped.Struct(yEntries))
            if structTypednessMatch == StructTypednessMatch.no ⇒
            structEntriesEq.eqv(xEntries, yEntries)
          case _ ⇒ false  // different datatype-types
        }
    }
}

sealed trait StructTypednessMatch
object StructTypednessMatch {
  case object yes extends StructTypednessMatch
  case object  no extends StructTypednessMatch
  implicit val default: StructTypednessMatch = yes
}
