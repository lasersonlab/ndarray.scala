package org.lasersonlab.ndarray

import cats.implicits._
import cats.{ Id, Traverse }
import lasersonlab.shapeless.slist._
import org.lasersonlab.ndarray.Indices.Idx
import org.lasersonlab.ndarray.Vectors._

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

  implicit val indices_0: Indices[Id, `0`] = new Indices[Id, `0`] { override def apply(⊥ : `0`[Int]): ⊥ = ⊥ }

  // NOTE: the compiler doesn't use this to derive instances for `Vectors` types
  implicit def cons[TL[_], Row[_]](
    implicit
    e: Indices[Row, TL],
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
                .toVector
                .map {
                  i ⇒
                    e(t).map { i :: _ }
                }
            )
        }
    }

  // Vector CanBuildFrom
  import hammerlab.collection._

  // The auto-derivations needed below fail, seemingly due to unification issues / general inability to work with HKTs,
  // so here are some manual instances:

  implicit val indices1  =
    new Indices[Vector1, `1`] {
      def apply(shape: Shape): Vector1[Index] =
        shape match {
          case n :: ⊥ ⇒
            (0 until n).map[Shape, Vector[Index]] { _ :: ⊥ }
        }
    }

  // TODO: this crashes the compiler if it is searched for via `cons` above
  implicit val indices2 = cons[`1`, Vector1]
  implicit val indices3 = cons[`2`, Vector2]
  implicit val indices4 = cons[`3`, Vector3]
  implicit val indices5 = cons[`4`, Vector4]
  implicit val indices6 = cons[`5`, Vector5]

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
