package org.lasersonlab.zarr

import cats.{ Semigroupal, Traverse }
import org.lasersonlab.ndarray.Vectors._
import org.lasersonlab.ndarray.{ ArrayLike, Scannable }
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
  implicit def cds: Decoder[ShapeT[Chunk.Idx]]
  implicit def es: Encoder[Shape]
  implicit def ces: Encoder[ShapeT[Chunk.Idx]]
  implicit def ti: Indices.Aux[A, ShapeT[Chunk.Idx]]
  implicit def traverse: Traverse[A]
  implicit def traverseShape: Traverse[ShapeT]
  implicit def semigroupalShape: Semigroupal[ShapeT]
  implicit def scannable: Scannable[ShapeT]
  implicit def arrayLike: ArrayLike.Aux[A, ShapeT[Chunk.Idx]]
}
object VectorInts {

  import org.lasersonlab.shapeless.Shape

  type Ax[N <: Nat, S[_], Idx] = VectorInts[N, Idx] { type ShapeT[U] = S[U] }

  //type Idx = Int

  type Aux[N <: Nat, S[_], Idx, _A[_]] =
    VectorInts[N, Idx] {
      type ShapeT[U] = S[U]
      type A[U] = _A[U]
    }

  def make[N <: Nat, S[_], Idx, _A[_]](
    implicit
    _ds: Decoder[S[Idx]],
    _cds: Decoder[S[Chunk.Idx]],
    _es: Encoder[S[Idx]],
    _ces: Encoder[S[Chunk.Idx]],
    _ti: Indices.Aux[_A, S[Chunk.Idx]],
    _traverse: Traverse[_A],
    _traverseShape: Traverse[S],
    _semigroupalShape: Semigroupal[S],
    _scannable: Scannable[S],
    _arrayLike: ArrayLike.Aux[_A, S[Chunk.Idx]]
  ):
    Aux[N, S, Idx, _A] =
    new VectorInts[N, Idx] {
      type ShapeT[U] = S[U]
      type A[U] = _A[U]

      override implicit val ds = _ds
      override implicit val cds = _cds
      override implicit val es = _es
      override implicit val ces = _ces
      override implicit val ti = _ti
      override implicit val traverse = _traverse
      override implicit val traverseShape = _traverseShape
      override implicit val semigroupalShape = _semigroupalShape
      override implicit val scannable = _scannable
      implicit val arrayLike = _arrayLike
    }

  import cats.implicits._
  import Shape.instances._
  import shapeless.nat._
  implicit val `1` = make[_1, Shape._1, Int, Vector1]
  implicit val `2` = make[_2, Shape._2, Int, Vector2]
  implicit val `3` = make[_3, Shape._3, Int, Vector3]
  implicit val `4` = make[_4, Shape._4, Int, Vector4]
  implicit val `5` = make[_5, Shape._5, Int, Vector5]
  implicit val `6` = make[_6, Shape._6, Int, Vector6]

  // TODO: allow constructing from a Seq[Int]?
  //def fromSeq(ints: Seq[Int]): Option[VectorInts]
}
