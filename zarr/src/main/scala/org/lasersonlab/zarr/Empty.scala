package org.lasersonlab.zarr

import shapeless._

/**
 * Type-level default / "fill" values for Zarr data-types
 */
sealed trait Empty[T] {
  def apply(): T
}
object Empty {
  def apply[T]()(implicit e: Empty[T]) = e()

  def make[T](t: T): Empty[T] = new Empty[T] { def apply(): T = t }

  implicit val    int: Empty[   Int] = make(0)
  implicit val   long: Empty[  Long] = make(0)
  implicit val  short: Empty[ Short] = make(0)
  implicit val  float: Empty[ Float] = make(0.0f)
  implicit val double: Empty[Double] = make(0)
  implicit val string: Empty[String] = make("")

  implicit val hnil: Empty[HNil] = make(HNil)
  implicit def cons[H, T <: HList](
    implicit
    h: Lazy[Empty[H]],
    t: Lazy[Empty[T]]
  ):
    Empty[H :: T] =
    make(
      h.value() ::
      t.value()
    )
  implicit def caseclass[T, L <: HList](
    implicit
    g: Generic.Aux[T, L],
    l: Lazy[Empty[L]]
  ):
    Empty[T] =
    make(
      g.from(
        l.value()
      )
    )
}
