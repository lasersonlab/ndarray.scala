package org.lasersonlab.zarr

import cats.Traverse
import lasersonlab.shapeless.slist._
import org.lasersonlab.circe.{ CodecK, DecoderK, EncoderK }
import org.lasersonlab.ndarray.ArrayLike
import org.lasersonlab.ndarray.Vectors._
import org.lasersonlab.shapeless.{ Scannable, Zip }
import org.lasersonlab.zarr.circe._
import org.lasersonlab.zarr.utils.Idx
import shapeless.Nat

/**
 * Type- and value-level function from a [[Nat type-level natural number]] [[N]] and index-type [[Idx]] to corresponding
 * types and implicit values for an [[N]]-dimensional [[Array]] with indices (along each dimension) of type [[Idx]]
 * (typically [[Int]], but potentially e.g. [[Long]])
 *
 * TODO: remove [[N]], parameterize by ShapeT and [[Idx]]
 */
trait VectorEvidence[N <: Nat, Idx] {
  /**
   * Type-constructor for the "shape" of the array (and its indices)
   *
   * Apply an [[Idx "index"]] type (e.g. [[Int]] or [[Long]]) to get the actual shape/index type
   *
   * Examples: [[List]], [[`2`]]/[[`3`]]/etc.
   */
  type ShapeT[_]

  /**
   * Type-constructor for an N-dimensional [[Array]] (which directly stores chunks); apply a "chunk"-type to get the N-D
   * array of chunks
   */
  type A[_]

  /**
   * Alias for the actual shape/index type of the [[Array]]: [[ShapeT the "shape" constructor]] applied to
   * [[Idx the "index" type]]
   */
  type Shape = ShapeT[Idx]

  implicit val idx: utils.Idx.T[Idx]
  implicit def shapeCodec: CodecK[ShapeT]
  implicit def idec: Decoder[Idx]
  implicit def ti: Indices.Aux[A, ShapeT]
  implicit def traverse: Traverse[A]
  implicit def traverseShape: Traverse[ShapeT]
  implicit def zipShape: Zip[ShapeT]
  implicit def scannable: Scannable[ShapeT]
  implicit def arrayLike: ArrayLike.Aux[A, ShapeT]
}
object VectorEvidence {

  type Ax[N <: Nat, S[_], Idx] = VectorEvidence[N, Idx] { type ShapeT[U] = S[U] }

  type Aux[N <: Nat, S[_], Idx, _A[_]] =
    VectorEvidence[N, Idx] {
      type ShapeT[U] = S[U]
      type A[U] = _A[U]
    }

  def make[N <: Nat, S[_], Idx, _A[_]](
    implicit
    _shapeCodec: CodecK[S],
    _idec: Decoder[Idx],
    _idx: Idx.T[Idx],
    _ti: Indices.Aux[_A, S],
    _traverse: Traverse[_A],
    _traverseShape: Traverse[S],
    _zipShape: Zip[S],
    _scannable: Scannable[S],
    _arrayLike: ArrayLike.Aux[_A, S]
  ):
    Aux[N, S, Idx, _A] =
    new VectorEvidence[N, Idx] {
      type ShapeT[U] = S[U]
      type A[U] = _A[U]

      override implicit val  idx = _idx
      override implicit val idec = _idec
      override implicit val shapeCodec = _shapeCodec
      override implicit val ti = _ti
      override implicit val traverse = _traverse
      override implicit val traverseShape = _traverseShape
      override implicit val zipShape = _zipShape
      override implicit val scannable = _scannable
      override implicit val arrayLike = _arrayLike
    }

  import cats.implicits._
  import lasersonlab.shapeless.slist._
  import shapeless.nat._

  implicit val int = Idx.Int

  implicit val `1` = make[_1, `1`, Int, Vector1]
  implicit val `2` = make[_2, `2`, Int, Vector2]
  implicit val `3` = make[_3, `3`, Int, Vector3]
  implicit val `4` = make[_4, `4`, Int, Vector4]
  implicit val `5` = make[_5, `5`, Int, Vector5]
  implicit val `6` = make[_6, `6`, Int, Vector6]
}
