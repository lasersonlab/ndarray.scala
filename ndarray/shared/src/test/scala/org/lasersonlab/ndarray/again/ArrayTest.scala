package org.lasersonlab.ndarray.again

import org.lasersonlab.ndarray.again.Array.Arg

class ArrayTest
  extends hammerlab.Suite {
  test("1-D") {
    val a = Array(1 to 10 toVector)

    a should be(Cons((1 to 10 toVector) map(Atom(_))))
  }

  test("2-D") {

    !![Arg[Vector[Int]]]
    !![Arg[Vector[Vector[Int]]]]

    !![Arg.Ax[Vector[Int], Int]]
    !![Arg.Ax[Vector[Vector[Int]], Int]]

    Arg.cons[Vector[Int], Int]

//    !![Arg.Aux[Vector[Int], Int, Cons[?, Atom]]]

//    !![Arg.Aux[Vector[Vector[Int]], Int, Cons[?, Cons[?, Atom]]]]
//    !![Arg.Aux[Vector[Vector[Int]], Vector[Int], Cons[?, Atom]]]
    !![Arg.Aux[Vector[Vector[Int]], Vector[Vector[Int]], Atom]]

//    !![Arg.Aux[Vector[Vector[Int]], Int, λ[A1 ⇒ Cons[A1, λ[A2 ⇒ Cons[A2, Atom]]]]]]

    val a =
      Array(
        //Vector()
        Vector(
          10 to 20 toVector,
          30 to 40 toVector
        )/*,
        Vector(
          50 to 60 toVector,
          70 to 80 toVector
        )*/
      )

    a should be(
      Cons(
        Vector(
          Cons(
            (10 to 20 toVector).map(Atom(_))
          ),
          Cons(
            (30 to 40 toVector).map(Atom(_))
          )
        )/*,
        Vector(
          Cons(
            50 to 60 toVector
          ),
          Cons(
            70 to 80 toVector
          )
        )*/
      )
    )
  }

  test("3-D") {
    val a =
      Array(
        Vector(
          Vector(
            10 to 20 toVector,
            30 to 40 toVector
          ),
          Vector(
            50 to 60 toVector,
            70 to 80 toVector
          )
        )
      )

    a should be(
      Cons(
        Vector(
          Cons(
            Vector(
              Cons(
                (10 to 20 toVector).map(Atom(_))
              ),
              Cons(
                (30 to 40 toVector).map(Atom(_))
              )
            )
          ),
          Cons(
            Vector(
              Cons(
                (50 to 60 toVector).map(Atom(_))
              ),
              Cons(
                (70 to 80 toVector).map(Atom(_))
              )
            )
          )
        )
      )
    )
  }
}
