package org.lasersonlab.zarr

import cats.{ Id, Traverse }
import lasersonlab.shapeless.slist._
import org.lasersonlab.ndarray.Vectors
import org.lasersonlab.ndarray.Vectors.{ Aux ⇒ _, _ }
import org.lasersonlab.zarr.Indices.Idx
import shapeless.Lazy

/**
 * Generate an N-dimensional array (ot type [[A]]) filled with N-dimensional indices (of type [[ShapeT]])
 */
trait Indices[A[_], ShapeT[_]] {
  type Shape = ShapeT[Idx]
  type Index = ShapeT[Idx]
  /**
   * Generate all lattice points in the hyperrectangle between the origin (inclusive) and a provided [[ShapeT]]
   * (exclusive)
   */
  def apply(shape: Shape): A[Index]
}

object Indices {
  type Idx = Int

  implicit val indices_0: Indices[Id, `0`] =
    new Indices[Id, `0`] {
      override def apply(⊥ : `0`[Int]): ⊥ = ⊥
    }

  // NOTE: the compiler doesn't use this to derive instances for `Vectors` types
  implicit def cons[TL[_], Row[_]](
    implicit
    e: Lazy[Indices[Row, TL]],
    f: Traverse[Row],
    cons: Cons[TL]
  ):
  Indices[
    Vectors.Aux[Row, ?],
    cons.Out
  ] =
    new Indices[Vectors.Aux[Row, ?], cons.Out] {
      def apply(shape: Shape): Vectors.Aux[Row, Index] =
        shape match {
          case h :: t ⇒
            Vectors.make[Row, Index](
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

  implicit val     indices1:      Indices[Vector1, `1`]  =
  new Indices[Vector1, `1`] {
    def apply(shape: Shape): Vector1[Index] =
      shape match {
        case n :: ⊥ ⇒
          (0 until n).map[Shape, Vector[Index]]{ _ :: ⊥ }
      }
  }

  // TODO: this crashes the compiler if it is searched for via `cons` above
  implicit val lazyIndices1: Lazy[Indices[Vector1, `1`]] = Lazy(indices1)

  implicit val     indices2:      Indices[Vector2, `2`]  = Indices.cons[`1`, Vector1]
  implicit val lazyIndices2: Lazy[Indices[Vector2, `2`]] = Lazy(indices2)

  implicit val     indices3:      Indices[Vector3, `3`]  = Indices.cons[`2`, Vector2]
  implicit val lazyIndices3: Lazy[Indices[Vector3, `3`]] = Lazy(indices3)

  implicit val     indices4:      Indices[Vector4, `4`]  = Indices.cons[`3`, Vector3]
  implicit val lazyIndices4: Lazy[Indices[Vector4, `4`]] = Lazy(indices4)

  implicit val     indices5:      Indices[Vector5, `5`]  = Indices.cons[`4`, Vector4]
  implicit val lazyIndices5: Lazy[Indices[Vector5, `5`]] = Lazy(indices5)

  implicit val     indices6:      Indices[Vector6, `6`]  = Indices.cons[`5`, Vector5]
  implicit val lazyIndices6: Lazy[Indices[Vector6, `6`]] = Lazy(indices6)

//  implicit val seq: Indices[Seq, Seq[Int]] =
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
