package org.lasersonlab.zarr.dtype

import cats.Eq
import org.lasersonlab.zarr.dtype.DataType._
import shapeless.the

/**
 * Instances for comparing two [[DataType]]s, whether they are known to have the same underlying type or not
 */
trait EqInstances {

  lazy val structEq: Eq[struct.?] =
    Eq.by[
      struct.?,
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

  implicit def auxEq[T]: Eq[DataType[T]] =
    new Eq[DataType[T]] {
      def eqv(x: DataType[T], y: DataType[T]): Boolean = dataTypeEq.eqv(x, y)
    }

  implicit lazy val dataTypeEq: Eq[DataType.?] =
    new Eq[DataType.?] {
      import cats.derived.auto.eq._
      import cats.implicits.catsKernelStdOrderForInt
      val primitive = the[Eq[Primitive[_]]]

      def eqv(x: DataType.?, y: DataType.?): Boolean =
        (x, y) match {
          case (x: Primitive[_], y: Primitive[_]) ⇒ primitive.eqv(x, y)
          case (x: struct.?, y: struct.?) ⇒ structEq.eqv(x, y)
          case (
            struct(
              StructList(
                xEntries,
                xSize
              )
            ),
            struct(
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
