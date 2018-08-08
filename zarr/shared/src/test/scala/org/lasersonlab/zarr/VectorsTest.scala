package org.lasersonlab.zarr

import cats.{ Foldable, Id, Traverse }
import cats.implicits._
import cats.syntax.foldable
//import org.lasersonlab.zarr.Vectors.IsVectors

class VectorsTest
  extends hammerlab.Suite {
  test("indices") {
    val arr =
      Vectors(
        Vector(
          10 to 20 toVector,
          30 to 40 toVector
        ),
        Vector(
          50 to 60 toVector,
          70 to 80 toVector
        )
      )

    !![Traverse[Id]]

    import Vectors.traverse

    Vectors.traverse
    Vectors.traverse[Id]

    !![Traverse[λ[A ⇒ Vectors[A, Id]]]]
    !![Traverse[Vectors[?, Id]]]
    !![Foldable[Vectors[?, Id]]]

    Vectors.traverse[Vector]

    !![Traverse[λ[A ⇒ Vectors[A, Vector]]]]
    !![Traverse[Vectors[?, Vector]]]
    !![Foldable[Vectors[?, Vector]]]

//    arr.foldl(0)(_ + _) should be(0)
  }
}
