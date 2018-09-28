package org.lasersonlab.zarr

import cats.Foldable

object Key {
  def apply[F[_]: Foldable, Idx](fi: F[Idx]): String = fi.toList.mkString(".")
}
