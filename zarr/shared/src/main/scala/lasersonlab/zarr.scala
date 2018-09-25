package lasersonlab

import org.lasersonlab.{ zarr ⇒ z }

object zarr {
  type Group = z.Group[Int]
  object Group {
    type Long = z.Group[scala.Long]
  }

  type Array[ShapeT[_], T] = z.Array.Of[ShapeT, Int, T]
  object Array {
    type Long[ShapeT[_], T] = z.Array.Of[ShapeT, T, scala.Long]
  }
}
