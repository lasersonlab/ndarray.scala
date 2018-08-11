package org.lasersonlab.zarr

import cats.{ Foldable, Id, Traverse }
import cats.implicits._
import cats.syntax.foldable
//import org.lasersonlab.zarr.Vectors.IsVectors

class VectorsTest
  extends hammerlab.Suite {
  test("indices") {
    val arr =
      Vctrs(
        Vector(
          10 to 20 toVector,
          30 to 40 toVector
        ),
        Vector(
          50 to 60 toVector,
          70 to 80 toVector
        )
      )



//    !![Traverse[Id]]
//
//    import Vectors.traverse
//
//    Vectors.traverse
//    Vectors.traverse[Id]
//
//    !![Traverse[λ[A ⇒ Vectors[A, Id]]]]
//    !![Traverse[Vectors[?, Id]]]
//    !![Foldable[Vectors[?, Id]]]
//
//    Vectors.traverse[Vector]
//
//    !![Traverse[λ[A ⇒ Vectors[A, Vector]]]]
//    !![Traverse[Vectors[?, Vector]]]
//    !![Foldable[Vectors[?, Vector]]]

//    !![Traverse[Vctrs.Aux[?, Id]]]
//    !![Traverse[Vctrs.Aux[?, Vector]]]
//    !![Traverse[Vctrs.Aux[?, λ[A ⇒ Vctrs.Aux[A, Id]]]]]
//    !![Traverse[Vctrs.Aux[?, Vctrs.Aux[?, λ[A ⇒ Vctrs.Aux[A, Id]]]]]]

    !![Traverse[Vctrs]]

    arr.foldLeft(0)(_ + _) should be(1980)

    arr.map(_ * 2).foldLeft(0)(_ + _) should be(3960)

    //implicit val t = Vectors.traverse[Vectors[?, λ[A ⇒ Vectors[A, Id]]]]

//    toFoldableOps[Vectors[?, Vectors[?, λ[A ⇒ Vectors[A, Id]]]], Int](arr).foldLeft(0)(_ + _) should be(1980)

//    val doubled = t.traverse[Id, Int, Int](arr)(_ * 2)

//    t.foldLeft(arr, 0)(_ + _) should be(1980)

//    t.foldLeft(doubled, 0)(_ + _) should be(3960)
//    (arr: Vectors[Int, Vectors[?, Vectors[?, Id]]])

//    arr.traverse

    //arr.traverseRow

//    arr.foldl(0)(_ + _) should be(0)
  }
}
