package org.lasersonlab.zarr

import cats.Traverse
import io.circe.Decoder
import org.lasersonlab.ndarray.Vectors._
import org.lasersonlab.ndarray.{ Arithmetic, ScanRight, Sum }
import org.lasersonlab.ndarray.Ints._
import shapeless.Nat

/**
 * Type- and value-level function from a [[Nat type-level natural number]] [[N]] to corresponding types and implicit
 * values for an [[N]]-dimensional [[Array]]
 */
trait VectorInts[N <: Nat] {
  type Shape
  type A[_]

  implicit def arithmetic: Arithmetic.Id[Shape]
  implicit def ds: Decoder[Shape]
  implicit def ti: Indices.Aux[A, Shape]
  implicit def traverse: Traverse[A]
  implicit def ai: Arithmetic[Shape, Int]
  implicit def key: Key[Shape]
  implicit def scanRight: ScanRight.Aux[Shape, Int, Int, Shape]
  implicit def sum: Sum.Aux[Shape, Int]
}
object VectorInts {
  type Aux[N <: Nat, _S, _A[_]] =
    VectorInts[N] {
      type Shape = _S
      type A[U] = _A[U]
    }

  def make[N <: Nat, _S, _A[_]](
    implicit
    _arithmetic: Arithmetic.Id[_S],
    _ds: Decoder[_S],
    _key: Key[_S],
    _ti: Indices.Aux[_A, _S],
    _traverse: Traverse[_A],
    _ai: Arithmetic[_S, Int],
    _scanRight: ScanRight.Aux[_S, Int, Int, _S],
    _sum: Sum.Aux[_S, Int]
  ):
    Aux[N, _S, _A] =
    new VectorInts[N] {
      type Shape = _S
      type A[U] = _A[U]

      implicit val arithmetic = _arithmetic
      implicit val ds = _ds
      implicit val key = _key
      implicit val ti = _ti
      implicit val traverse = _traverse
      implicit val ai = _ai
      implicit val scanRight = _scanRight
      implicit val sum = _sum
    }

  import cats.implicits._
  import shapeless.nat._

  implicit val `1` = make[_1, Ints1, Vector1]
  implicit val `2` = make[_2, Ints2, Vector2]
  implicit val `3` = make[_3, Ints3, Vector3]
  implicit val `4` = make[_4, Ints4, Vector4]
  implicit val `5` = make[_5, Ints5, Vector5]
  implicit val `6` = make[_6, Ints6, Vector6]
}
