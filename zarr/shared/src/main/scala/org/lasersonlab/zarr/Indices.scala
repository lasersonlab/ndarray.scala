package org.lasersonlab.zarr

import cats.implicits._
import cats.{ Id, Traverse }
import lasersonlab.shapeless.slist._
//import hammerlab.shapeless.tlist._
//import org.lasersonlab.ndarray.Ints._
import org.lasersonlab.ndarray.Vectors
import org.lasersonlab.ndarray.Vectors._
import shapeless.Lazy

/**
 * Iterate over an N-dimensional range of integers (provided as a `Shape`), stored as an [[A]]
 */
trait Indices[A[_]] {
  // TODO: make Shape a HKT, add Idx type
  type Shape
  def apply(shape: Shape): A[Shape]
}
object Indices {

  type Aux[A[_], _Shape] = Indices[A] { type Shape = _Shape }

  implicit val tnil: Aux[Id, ⊥] =
    new Indices[Id] {
      type Shape = ⊥
      def apply(tnil: ⊥): Id[⊥] = tnil
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
    Int :: TL
  ] =
    new Indices[Vectors.Aux[?, Row]] {
      type Shape = Int :: TL
      def apply(shape: Shape): Vectors.Aux[Shape, Row] =
        shape match {
          case h :: t ⇒
            Vectors.make[Shape, Row](
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

  implicit val     indices1:      Indices.Aux[Vector1, `1`[Int]]  =
  new Indices[Vector1] {
    type Shape = `1`[Int]
    def apply(shape: Shape): Vector1[Shape] =
      shape match {
        case n :: ⊥ ⇒
          (0 until n).map[Shape, Vector[Shape]]{ _ :: ⊥ }
      }
  }

  // TODO: this crashes the compiler if it is searched for via `cons` above
  implicit val lazyIndices1: Lazy[Indices.Aux[Vector1, `1`[Int]]] = Lazy(indices1)

  implicit val     indices2:      Indices.Aux[Vector2, `2`[Int]]  = Indices.cons[`1`[Int], Vector1]
  implicit val lazyIndices2: Lazy[Indices.Aux[Vector2, `2`[Int]]] = Lazy(indices2)

  implicit val     indices3:      Indices.Aux[Vector3, `3`[Int]]  = Indices.cons[`2`[Int], Vector2]
  implicit val lazyIndices3: Lazy[Indices.Aux[Vector3, `3`[Int]]] = Lazy(indices3)

  implicit val     indices4:      Indices.Aux[Vector4, `4`[Int]]  = Indices.cons[`3`[Int], Vector3]
  implicit val lazyIndices4: Lazy[Indices.Aux[Vector4, `4`[Int]]] = Lazy(indices4)

  implicit val     indices5:      Indices.Aux[Vector5, `5`[Int]]  = Indices.cons[`4`[Int], Vector4]
  implicit val lazyIndices5: Lazy[Indices.Aux[Vector5, `5`[Int]]] = Lazy(indices5)

  implicit val     indices6:      Indices.Aux[Vector6, `6`[Int]]  = Indices.cons[`5`[Int], Vector5]
  implicit val lazyIndices6: Lazy[Indices.Aux[Vector6, `6`[Int]]] = Lazy(indices6)

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
