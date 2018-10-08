package org.lasersonlab.ndarray

import cats.implicits._
import org.lasersonlab.ndarray.Indices.Idx

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
}
