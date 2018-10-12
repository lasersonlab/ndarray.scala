package org.lasersonlab.zarr.io

import hammerlab.either._
import hammerlab.option._
import hammerlab.path._
import io.circe.Encoder
import lasersonlab.xscala._
import org.lasersonlab.zarr.pprint
import shapeless.labelled.FieldType
import shapeless.{ Path ⇒ _, _ }

import scala.util.Try

trait Save[T] {
  def apply(t: T, dir: Path): Throwable | Unit
}

trait LowPrioritySave
  extends BasenameSave
{
  implicit val hnil: Save[HNil] = new Save[HNil] { def apply(t: HNil, dir: Path) = Right(()) }

  implicit def savecons[
    K <: Symbol,
        Head,
        Tail <: HList
  ](
    implicit
//    name: Witness.Aux[Basename],
//    head: Save[Head],
//    tail: Save[Tail],
    head: Lazy[Save[Head]],
    tail: Lazy[Save[Tail]]
  ):
    Save[
      FieldType[K, Head] ::
      Tail
    ] =
    ???
//    new Save[
//      FieldType[Basename, Head] ::
//      Tail
//    ] {
//      def apply(t: FieldType[Basename, Head] :: Tail, dir: Path): Throwable | Unit =
//        t match {
//          case h :: t ⇒
//            for {
////              _ ← head(h, dir / name.value.name)
////              _ ← tail(t, dir)
//              _ ← head.value(h, dir / name.value.name)
//              _ ← tail.value(t, dir)
//            } yield
//              ()
//        }
//    }

  implicit def caseclass[
    CC,
    L <: HList
  ](
    implicit
    g: LabelledGeneric.Aux[CC, L],
//    l: Save[L]
    l: Lazy[Save[L]]
  ):
    Save[CC] =
    new Save[CC] {
      def apply(t: CC, dir: Path): Throwable | Unit =
//        l(g.to(t), dir)
        l.value(g.to(t), dir)
    }
}

trait BasenameSave
{

  implicit def withBasenameAsJSON[T](
    implicit
    basename: Basename[T],
    encoder: Encoder[T]
  ):
    Save[T] =
    new Save[T] {
      def apply(t: T, dir: Path): Throwable | Unit =
        Try {
          val path = dir / basename
          path.mkdirs
          path
            .write(
              pprint(
                encoder(t)
              )
            )
        }
        .toEither
    }
}

object Save
  extends LowPrioritySave {

  implicit def opt[T](implicit save: Save[T]): Save[Opt[T]] =
    new Save[Opt[T]] {
      def apply(t: Opt[T], dir: Path): Throwable | Unit =
        t match {
          case Som(t) ⇒ save(t, dir)
          case _ ⇒ Right(())
        }
    }

  implicit class Ops[T](val t: T) extends AnyVal {
    def save(dir: Path)(implicit save: Save[T]): Throwable | Unit = save(t, dir)
  }

  trait syntax {
    @inline implicit def zarrSaveOps[T](t: T) = Ops(t)
  }
}
