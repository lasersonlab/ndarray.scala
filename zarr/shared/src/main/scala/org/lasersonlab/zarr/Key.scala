package org.lasersonlab.zarr

import cats.Foldable
import cats.implicits._

object Key {
  def apply[F[_]: Foldable, Idx](fi: F[Idx]): String = fi.toList.mkString(".")
}
