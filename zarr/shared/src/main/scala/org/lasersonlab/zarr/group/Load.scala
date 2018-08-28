package org.lasersonlab.zarr.group

import hammerlab.either._
import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder
import io.circe.parser.parse
import org.lasersonlab.zarr.FillValue.FillValueDecoder
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.untyped.Group
import org.lasersonlab.zarr.{ VectorInts, Array ⇒ Arr }
import shapeless.labelled._
import shapeless.{ Witness ⇒ W, Path ⇒ _, _ }

trait Load[T] {
  def apply(dir: Path): Exception | T
}

trait LowPriorityLoad {
  implicit def basename[T](
    implicit
    basename: Basename[T],
    decoder: Decoder[T]
  ):
    Load[T] =
    new Load[T] {
      def apply(dir: Path): Exception | T =
        dir ? basename flatMap {
          path ⇒
            import Decoder._
            parse(path.read)
              .flatMap {
                decoder.decodeJson
              }
        }
    }
}
object Load
  extends LowPriorityLoad {
  implicit def basenameOpt[T](
    implicit
    basename: Basename[T],
    decoder: Decoder[T]
  ):
    Load[Opt[T]] =
    new Load[Opt[T]] {
      def apply(dir: Path): Exception | Opt[T] =  {
        val path = dir / basename
        if (!path.exists)
          Right(None)
        else
          parse(path.read)
            .flatMap {
              json ⇒
                decoder
                  .decodeJson(json)
            }
            .map { o ⇒ o }
      }
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
    def load[T](implicit l: Load[T]): Exception | T = l(dir)
  }
}
