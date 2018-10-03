package org.lasersonlab.zarr

import cats.Traverse
import cats.implicits._
import lasersonlab.shapeless.slist._
import org.lasersonlab.circe.CodecK
import org.lasersonlab.ndarray.ArrayLike
import org.lasersonlab.ndarray.Vectors._
import org.lasersonlab.shapeless.{ Scannable, Zip }
import org.lasersonlab.zarr.utils.Idx

/**
 * Type- and value-level function from [[ShapeT a shape type]] to corresponding types and implicit values for an
 * N-dimensional [[Array]].
 *
 * @tparam ShapeT Type-constructor for the "shape" of the array (and its indices); apply an [[Idx "index"]] type (e.g.
 *                [[Int]] or [[Long]]) to get the actual shape/index type. Examples: [[List]], [[`2`]]/[[`3`]]/etc.
 */
trait VectorEvidence[ShapeT[_]] {
  /**
   * Type-constructor for an N-dimensional [[Array]] (which directly stores chunks); apply a "chunk"-type to get the N-D
   * array of chunks
   */
  type A[_]

  implicit def shapeCodec: CodecK[ShapeT]
  implicit def ti: Indices.Aux[A, ShapeT]
  implicit def traverse: Traverse[A]
  implicit def traverseShape: Traverse[ShapeT]
  implicit def zipShape: Zip[ShapeT]
  implicit def scannable: Scannable[ShapeT]
  implicit def arrayLike: ArrayLike.Aux[A, ShapeT]
}
object VectorEvidence {

  type Aux[S[_], _A[_]] = VectorEvidence[S] { type A[U] = _A[U] }

  def make[S[_], _A[_]](
    implicit
    _shapeCodec: CodecK[S],
    _ti: Indices.Aux[_A, S],
    _traverse: Traverse[_A],
    _traverseShape: Traverse[S],
    _zipShape: Zip[S],
    _scannable: Scannable[S],
    _arrayLike: ArrayLike.Aux[_A, S]
  ):
    Aux[S, _A] =
    new VectorEvidence[S] {
      type A[U] = _A[U]

      override implicit val    shapeCodec = _shapeCodec
      override implicit val            ti = _ti
      override implicit val      traverse = _traverse
      override implicit val traverseShape = _traverseShape
      override implicit val      zipShape = _zipShape
      override implicit val     scannable = _scannable
      override implicit val     arrayLike = _arrayLike
    }

  import lasersonlab.shapeless.slist._

  implicit val `1` = make[`1`, Vector1]
  implicit val `2` = make[`2`, Vector2]
  implicit val `3` = make[`3`, Vector3]
  implicit val `4` = make[`4`, Vector4]
  implicit val `5` = make[`5`, Vector5]
  implicit val `6` = make[`6`, Vector6]
}
