package org.lasersonlab.zarr

/**
 * Type-level default / "fill" values for Zarr data-types
 *
 * (Currently unused)
 */
sealed trait Empty[T] {
  def apply(): T
}
object Empty {
  def apply[T]()(implicit e: Empty[T]) = e()

  def make[T](t: T): Empty[T] = new Empty[T] { def apply(): T = t }
  implicit val    int: Empty[   Int] = make(0)
  implicit val   long: Empty[  Long] = make(0)
  implicit val  float: Empty[ Float] = make(0.0f)
  implicit val double: Empty[Double] = make(0)
  implicit val string: Empty[String] = make("")
}
