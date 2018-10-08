package org.lasersonlab.zarr

import cats.Traverse
import cats.implicits._
import lasersonlab.shapeless.slist._
import org.lasersonlab.circe.CodecK
import org.lasersonlab.ndarray.{ ArrayLike, FlatArray, Indices }
import org.lasersonlab.shapeless.{ Scannable, Zip }

/**
 * Type- and value-level function from [[ShapeT a shape type]] to corresponding types and implicit values for an
 * N-dimensional [[Array]].
 *
 * @tparam ShapeT Type-constructor for the "shape" of the array (and its indices); apply an "index" type (e.g. [[Int]]
 *                or [[Long]]) to get the actual shape/index type. Examples: [[List]], [[`2`]]/[[`3`]]/etc.
 */
trait VectorEvidence[ShapeT[_]] {
  /**
   * Type-constructor for an N-dimensional [[Array]] (which directly stores chunks); apply a "chunk"-type to get the N-D
   * array of chunks
   */
  type A[_]

  implicit val      traverse:  Traverse    [A        ]
  implicit val            ti:   Indices    [A, ShapeT]
  implicit val     arrayLike: ArrayLike.Aux[A, ShapeT]
  implicit val      zipShape:       Zip    [   ShapeT]
  implicit val     scannable: Scannable    [   ShapeT]
  implicit val    shapeCodec:    CodecK    [   ShapeT]
  implicit val traverseShape:  Traverse    [   ShapeT]
}
object VectorEvidence {

  type Aux[S[_], _A[_]] = VectorEvidence[S] { type A[U] = _A[U] }

  case class make[ShapeT[_], _A[_]](
    implicit
    val      traverse:  Traverse    [_A        ],
    val            ti:   Indices    [_A, ShapeT],
    val     arrayLike: ArrayLike.Aux[_A, ShapeT],
    val      zipShape:       Zip    [    ShapeT],
    val     scannable: Scannable    [    ShapeT],
    val    shapeCodec:    CodecK    [    ShapeT],
    val traverseShape:  Traverse    [    ShapeT],
  )
  extends VectorEvidence[ShapeT] {
    type A[U] = _A[U]
  }

  import lasersonlab.shapeless.slist._

  trait flat {
    implicit val flat1 = make[`1`, FlatArray.`1`]
    implicit val flat2 = make[`2`, FlatArray.`2`]
    implicit val flat3 = make[`3`, FlatArray.`3`]
    implicit val flat4 = make[`4`, FlatArray.`4`]
    implicit val flat5 = make[`5`, FlatArray.`5`]
    implicit val flat6 = make[`6`, FlatArray.`6`]
  }
  object flat extends flat
}
