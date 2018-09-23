package org.lasersonlab.zarr

import cats.{ Semigroupal, Traverse }
import org.lasersonlab.ndarray.Vectors._
import org.lasersonlab.ndarray.{ ArrayLike, Scannable }
import org.lasersonlab.shapeless.Zip
import org.lasersonlab.zarr.circe._
import org.lasersonlab.zarr.utils.slist.{ HKTDecoder, HKTEncoder }
import shapeless.Nat

/**
 * Type- and value-level function from a [[Nat type-level natural number]] [[N]] to corresponding types and implicit
 * values for an [[N]]-dimensional [[Array]]
 */
trait VectorInts[N <: Nat, Idx] {
  type ShapeT[_]
  type A[_]
  type Shape = ShapeT[Idx]

  implicit def sdec: HKTDecoder[ShapeT]
  implicit def idec: Decoder[Idx]
  implicit def senc: HKTEncoder[ShapeT]
  implicit def ienc: Encoder[Idx]
  implicit def ti: Indices.Aux[A, ShapeT]
  implicit def traverse: Traverse[A]
  implicit def traverseShape: Traverse[ShapeT]
  implicit def zipShape: Zip[ShapeT]
  implicit def scannable: Scannable[ShapeT]
  implicit def arrayLike: ArrayLike.Aux[A, ShapeT]
}
object VectorInts {

  type Ax[N <: Nat, S[_], Idx] = VectorInts[N, Idx] { type ShapeT[U] = S[U] }

  type Aux[N <: Nat, S[_], Idx, _A[_]] =
    VectorInts[N, Idx] {
      type ShapeT[U] = S[U]
      type A[U] = _A[U]
    }

  def make[N <: Nat, S[_], Idx, _A[_]](
    implicit
    _sdec: HKTDecoder[S],
    _idec: Decoder[Idx],
    _senc: HKTEncoder[S],
    _ienc: Encoder[Idx],
    _ti: Indices.Aux[_A, S],
    _traverse: Traverse[_A],
    _traverseShape: Traverse[S],
    _zipShape: Zip[S],
    _scannable: Scannable[S],
    _arrayLike: ArrayLike.Aux[_A, S]
  ):
    Aux[N, S, Idx, _A] =
    new VectorInts[N, Idx] {
      type ShapeT[U] = S[U]
      type A[U] = _A[U]

      override implicit val sdec = _sdec
      override implicit val idec = _idec
      override implicit val senc = _senc
      override implicit val ienc = _ienc
      override implicit val ti = _ti
      override implicit val traverse = _traverse
      override implicit val traverseShape = _traverseShape
      override implicit val zipShape = _zipShape
      override implicit val scannable = _scannable
      implicit val arrayLike = _arrayLike
    }

  import cats.implicits._
  //import Shape.instances._
  import lasersonlab.shapeless.slist._
  import shapeless.nat._
  implicit val `1` = make[_1, `1`, Int, Vector1]
  implicit val `2` = make[_2, `2`, Int, Vector2]
  implicit val `3` = make[_3, `3`, Int, Vector3]
  implicit val `4` = make[_4, `4`, Int, Vector4]
  implicit val `5` = make[_5, `5`, Int, Vector5]
  implicit val `6` = make[_6, `6`, Int, Vector6]

  // TODO: allow constructing from a Seq[Int]?
  //def fromSeq(ints: Seq[Int]): Option[VectorInts]
}
