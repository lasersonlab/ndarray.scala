package org.lasersonlab.zarr.dtype

import cats.Eq
import org.lasersonlab.zarr.dtype.DataType._
import shapeless.the

/**
 * Instances for comparing two [[DataType]]s, whether they are known to have the same underlying type or not
 */
trait EqInstances {

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

  implicit def auxEq[T]: Eq[Aux[T]] =
    new Eq[Aux[T]] {
      def eqv(x: Aux[T], y: Aux[T]): Boolean = dataTypeEq.eqv(x, y)
    }

  implicit lazy val dataTypeEq: Eq[DataType] =
    new Eq[DataType] {
      import cats.derived.auto.eq._
      import cats.implicits.catsKernelStdOrderForInt
      val primitive = the[Eq[Primitive]]

      def eqv(x: DataType, y: DataType): Boolean =
        (x, y) match {
          case (x: Primitive, y: Primitive) ⇒ primitive.eqv(x, y)
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
          case _ ⇒ false  // different datatype-types
        }
    }
}
