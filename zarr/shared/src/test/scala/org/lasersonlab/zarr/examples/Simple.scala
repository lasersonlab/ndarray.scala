package org.lasersonlab.zarr.examples

/**
 * Scratch-work related to code-samples used in README; can be tested via:
 *
 * {{{
 * sbt zarr/test:run
 * }}}
 */
object Simple {
  def main(args: Array[String]): Unit = {

    import lasersonlab.zarr._  // bring all things zarr into scope

    // Schema for a type that we will instatiate and then save as a Zarr "group" (a top-level directory with
    // subdirectories for each "array" field
    case class Foo(
         ints: Array[`2`,    Int],  // 2-D array of ints
      doubles: Array[`3`, Double]   // 3-D array of doubles
    )

    // Instantiate a Foo
    val foo =
      Foo(
           ints = Array(      1000 :: 1000 :: ⊥)( 1 to 1000000                 ),
        doubles = Array(100 :: 100 ::  100 :: ⊥)((1 to 1000000).map(_.toDouble))
      )

    // Save as a Zarr group: a directory containing "ints" and "doubles" subdirectories, each a Zarr array:
    foo.save(Path("foo"))

    {
      implicit val chunkSize: ChunkSize = 1 MB

      val foo =
        Foo(
             ints = Array(      1000 :: 1000 :: ⊥)( 1 to 1000000                 ),
          doubles = Array(100 :: 100 ::  100 :: ⊥)((1 to 1000000).map(_.toDouble))
        )

      foo.save(Path("foo.1m"))
    }

    {
      val foo = Path("foo").load[Foo].right.get
      println(foo.ints.foldLeft(0)(_ + _))
      val ints = foo.ints.t.toList
      println(ints.take(100))
      println(ints.takeRight(100))
    }

    {
      val foo = Path("foo.1m").load[Foo].right.get
      println(foo.ints.foldLeft(0)(_ + _))
      val ints = foo.ints.t.toList
      println(ints.take(100))
      println(ints.takeRight(100))
    }

    println(foo.ints(123 :: 456 :: ⊥))
    println(foo.doubles.foldLeft(0.0)(_ + _))
  }
}
