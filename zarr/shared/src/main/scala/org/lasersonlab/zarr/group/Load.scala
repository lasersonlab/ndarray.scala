package org.lasersonlab.zarr.group

import hammerlab.either._
import hammerlab.path._
import shapeless.labelled._
import shapeless.{ Path ⇒ _, _ }

trait Load[T] {
  def apply(dir: Path): Exception | T
}
object Load {

  implicit val hnil: Load[HNil] = new Load[HNil] {
    def apply(dir: Path): Exception | HNil = Right(HNil)
  }

  implicit def cons[K <: Symbol, H, T <: HList](
    implicit
    h: Lazy[Load[H]],
    t: Lazy[Load[T]],
    w: Witness.Lt[K]
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
}
