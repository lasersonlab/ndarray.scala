package org.lasersonlab.zarr.group

import cats.Foldable
import hammerlab.either._
import hammerlab.path._
import io.circe.Decoder
import org.lasersonlab.ndarray.Bytes
import org.lasersonlab.ndarray.Ints.Ints1
import org.lasersonlab.zarr
import org.lasersonlab.zarr.FillValue.FillValueDecoder
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.untyped.Group
import shapeless.labelled._
import shapeless.{ Path ⇒ _, Witness ⇒ W, _ }
import zarr.{ VectorInts, Array ⇒ Arr }

trait Load[T] {
  def apply(dir: Path): Exception | T
}
trait LowPriLoad {
//  implicit def cons[H, T <: HList](
//    implicit
//    h: Lazy[Load[H]],
//    t: Lazy[Load[T]]
//  ):
//    Load[H :: T] =
//    new Load[H :: T] {
//      def apply(dir: Path):
//        Exception |
//        (H :: T)
//      = {
//        for {
//          h ← h.value(dir / w.value)
//          t <- t.value(dir)
//        } yield
//          field[K](h) :: t
//      }
//    }
}
object Load
  extends LowPriLoad {

//  implicit def unshaped[T, N <: Nat](
//    implicit
//    v: VectorInts[N],
//    d: Decoder[DataType.Aux[T]],
//    dt: FillValueDecoder[T]
//  ):
//    Load[
//      Arr[T]
//    ] =
//    new Load[Arr[T]] {
//      override def apply(dir: Path): Exception | Arr[T] =
//        Arr[T, N](dir)
//    }

  implicit def array[T, N <: Nat, Shape](
    implicit
    v: VectorInts.Ax[N, Shape],
    d: Decoder[DataType.Aux[T]],
    dt: FillValueDecoder[T]
  ):
    Load[
      Arr.S[Shape, T]
    ] =
    new Load[Arr.S[Shape, T]] {
      override def apply(dir: Path): Exception | Arr.S[Shape, T] =
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
    h: Load[H],
    t: Load[T],
    w: W.Aux[K]
  ):
    Load[FieldType[K, H] :: T] =
    new Load[FieldType[K, H] :: T] {
      def apply(dir: Path):
        Exception |
        (
          FieldType[K, H] ::
          T
        ) = {
          for {
            h ← h(dir / w.value)
            t ← t(dir)
          } yield
            field[K](h) :: t
        }
    }

  implicit def caseclass[T, L <: HList](
    implicit
    lg: LabelledGeneric.Aux[T, L],
    load: Load[L]
  ):
    Load[T] =
    new Load[T] {
      def apply(dir: Path): Exception | T =
        load(dir).map { lg.from }
    }

  implicit class Ops(val dir: Path) extends AnyVal {
    def load[T](implicit l: Load[T]) = l(dir)
  }
}
