package org.lasersonlab.zarr

import cats.{ Id, Traverse }
import lasersonlab.shapeless.slist._
import org.lasersonlab.ndarray.Vectors
import org.lasersonlab.ndarray.Vectors._
import shapeless.Lazy

/**
 * Iterate over an N-dimensional range of integers (provided as a `Shape`), stored as an [[A]]
 */
trait Indices[A[_]] {
  type Shape[_]
  def apply(shape: Shape[Int]): A[Shape[Int]]
}
object Indices {

  type Aux[A[_], _Shape[_]] = Indices[A] { type Shape[U] = _Shape[U] }

  implicit val indices_0: Aux[Id, `0`] =
    new Indices[Id] {
      type Shape[T] = `0`[T]
      override def apply(⊥ : `0`[Int]): ⊥ = ⊥
    }

  // NOTE: the compiler doesn't use this to derive instances for `Vectors` types
  implicit def cons[TL[_], Row[_]](
    implicit
    e: Lazy[Aux[Row, TL]],
    f: Traverse[Row],
    cons: Cons[TL]
  ):
  Aux[
    Vectors.Aux[?, Row],
    cons.Out
  ] =
    new Indices[Vectors.Aux[?, Row]] {
      type Shape[U] = cons.Out[U]
      def apply(shape: Shape[Int]): Vectors.Aux[Shape[Int], Row] =
        shape match {
          case h :: t ⇒
            Vectors.make[Shape[Int], Row](
              (0 until h)
                .map {
                  i ⇒
                    f.map(
                      e.value(t)
                    ) {
                      i :: _
                    }
                }
                .toVector
            )
        }
    }

  // Vector CanBuildFrom
  import hammerlab.collection._

  // The auto-derivations needed below fail, seemingly due to unification issues / general inability to work with HKTs,
  // so here are some manual instances:

  implicit val     indices1:      Indices.Aux[Vector1, `1`]  =
  new Indices[Vector1] {
    type Shape[T] = `1`[T]
    def apply(shape: Shape[Int]): Vector1[Shape[Int]] =
      shape match {
        case n :: ⊥ ⇒
          (0 until n).map[Shape[Int], Vector[Shape[Int]]]{ _ :: ⊥ }
      }
  }

  // TODO: this crashes the compiler if it is searched for via `cons` above
  implicit val lazyIndices1: Lazy[Indices.Aux[Vector1, `1`]] = Lazy(indices1)

  implicit val     indices2:      Indices.Aux[Vector2, `2`]  = Indices.cons[`1`, Vector1]
  implicit val lazyIndices2: Lazy[Indices.Aux[Vector2, `2`]] = Lazy(indices2)

  implicit val     indices3:      Indices.Aux[Vector3, `3`]  = Indices.cons[`2`, Vector2]
  implicit val lazyIndices3: Lazy[Indices.Aux[Vector3, `3`]] = Lazy(indices3)

  implicit val     indices4:      Indices.Aux[Vector4, `4`]  = Indices.cons[`3`, Vector3]
  implicit val lazyIndices4: Lazy[Indices.Aux[Vector4, `4`]] = Lazy(indices4)

  implicit val     indices5:      Indices.Aux[Vector5, `5`]  = Indices.cons[`4`, Vector4]
  implicit val lazyIndices5: Lazy[Indices.Aux[Vector5, `5`]] = Lazy(indices5)

  implicit val     indices6:      Indices.Aux[Vector6, `6`]  = Indices.cons[`5`, Vector5]
  implicit val lazyIndices6: Lazy[Indices.Aux[Vector6, `6`]] = Lazy(indices6)

//  implicit val seq: Indices.Aux[Seq, Seq[Int]] =
//    new Indices[Seq] {
//      type Shape = Seq[Int]
//      @inline def apply(shape: Shape): Seq[Shape] = rec(shape.toList)
//      private def rec(shape: List[Int]): List[List[Int]] =
//        shape match {
//          case Nil ⇒ List(Nil)
//          case scala.::(h, t) ⇒
//            for {
//              h ← (0 until h).toList
//              t ← rec(t)
//            } yield
//              scala.::(h, t)
//        }
//    }
}
