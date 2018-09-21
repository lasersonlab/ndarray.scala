package org.lasersonlab.zarr

import cats.Traverse
import org.lasersonlab.ndarray.ArrayLike
import org.lasersonlab.ndarray.Ints._
import org.lasersonlab.ndarray.Vectors._
//import org.lasersonlab.zarr.VectorInts.Idx
import org.lasersonlab.zarr.circe._
import shapeless.Nat

/**
 * Type- and value-level function from a [[Nat type-level natural number]] [[N]] to corresponding types and implicit
 * values for an [[N]]-dimensional [[Array]]
 */
trait VectorInts[N <: Nat, Idx] {
  type ShapeT[_]
  type A[_]
  type Shape = ShapeT[Idx]

  implicit def ds: Decoder[Shape]
  implicit def es: Encoder[Shape]
  implicit def ti: Indices.Aux[A, Shape]
  implicit def traverse: Traverse[A]
  implicit def traverseShape: Traverse[ShapeT]
  implicit def arrayLike: ArrayLike.Aux[A, Shape]
}
object VectorInts {
  type Ax[N <: Nat, S[_], Idx] = VectorInts[N, Idx] { type Shape[U] = S[U] }

  type Idx = Int

  type Aux[N <: Nat, S[_], Idx, _A[_]] =
    VectorInts[N, Idx] {
      type Shape[U] = S[U]
      type A[U] = _A[U]
    }

  def make[N <: Nat, S[_], Idx, _A[_]](
    implicit
    _ds: Decoder[S[Idx]],
    _es: Encoder[S[Idx]],
    _ti: Indices.Aux[_A, S[Idx]],
    _traverse: Traverse[_A],
    _traverseShape: Traverse[S],
    _arrayLike: ArrayLike.Aux[_A, S[Idx]]
  ):
    Aux[N, S, Idx, _A] =
    new VectorInts[N, Idx] {
      type ShapeT[U] = S[U]
      type A[U] = _A[U]

      implicit val ds = _ds
      implicit val es = _es
      implicit val ti = _ti
      implicit val traverse = _traverse
      implicit val traverseShape: Traverse[ShapeT] = _traverseShape
      implicit val arrayLike = _arrayLike
    }: Aux[N, S, Idx, _A]

  import cats.implicits._
  import shapeless.nat._

  import org.lasersonlab.ndarray.TList
  implicit val `1` = make[_1, TList._1, Int, Vector1]
  implicit val `2` = make[_2, TList._2, Int, Vector2]
  implicit val `3` = make[_3, TList._3, Int, Vector3]
  implicit val `4` = make[_4, TList._4, Int, Vector4]
  implicit val `5` = make[_5, TList._5, Int, Vector5]
  implicit val `6` = make[_6, TList._6, Int, Vector6]

  // TODO: allow constructing from a Seq[Int]?
  //def fromSeq(ints: Seq[Int]): Option[VectorInts]
}
