package org.lasersonlab.zarr.group

import hammerlab.either._
import hammerlab.path._
import io.circe.Decoder
import org.lasersonlab.ndarray.Bytes
import org.lasersonlab.zarr
import org.lasersonlab.zarr.FillValue.FillValueDecoder
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.untyped.Group
import shapeless.labelled._
import shapeless.{ Path ⇒ _, _ }
import zarr.{ VectorInts, Array ⇒ Arr }

trait Load[T] {
  def apply(dir: Path): Exception | T
}
object Load {

  implicit def array[T, N <: Nat, Shape](
    implicit
    v: VectorInts.Ax[N, Shape],
    d: Decoder[DataType.Aux[T]],
    dt: FillValueDecoder[T]
  ):
    Load[
      Arr[T, Shape]
    ] =
    new Load[Arr[T, Shape]] {
      override def apply(dir: Path): Exception | Arr[T, Shape] =
        Arr[T, N](dir)
    }

  implicit val group: Load[Group] =
    new Load[Group] {
      override def apply(dir: Path): Exception | Group =
        Group(dir)
    }

  implicit val hnil: Load[HNil] = new Load[HNil] {
    def apply(dir: Path): Exception | HNil = Right(HNil)
  }

  implicit def cons[K <: Symbol, H, T <: HList](
    implicit
    h: Lazy[Load[H]],
    t: Lazy[Load[T]],
    w: Witness.Aux[K]
  ):
    Load[FieldType[K, H] :: T] =
    new Load[FieldType[K, H] :: T] {
      override def apply(dir: Path):
        Exception |
        (
          FieldType[K, H] ::
          T
        ) = {
          for {
            h ← h.value(dir / w.value)
            t <- t.value(dir)
          } yield
            field[K](h) :: t
        }
    }

  implicit def caseclass[T, L <: HList](
    implicit
    lg: LabelledGeneric.Aux[T, L],
    load: Lazy[Load[L]]
  ):
    Load[T] =
    new Load[T] {
      def apply(dir: Path): Exception | T =
        load.value(dir).map { lg.from }
    }

  implicit class Ops(val dir: Path) extends AnyVal {
    def load[T](implicit l: Load[T]) = l(dir)
  }
}
