# zarr.scala

[Zarr] implementation in Scala

Example usage:

```scala
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
```

At this point, the directory `foo` will look like:

```
foo
├── [         24]  .zgroup
├── [        128]  doubles
│   ├── [        368]  .zarray
│   └── [      58864]  0.0.0
└── [        128]  ints
    ├── [        346]  .zarray
    └── [      36267]  0.0
```

The `ints` and `doubles` arrays are each written out in one "chunk"-file; by default, chunks are comprised of as many "rows" as will fit in 32 MB (uncompressed).

This number can be tweaked by providing an implicit `ChunkSize`:

```scala
implicit val chunkSize: ChunkSize = 1 MB

val foo =
  Foo(
       ints = Array(      1000 :: 1000 :: ⊥)( 1 to 1000000                 ),
    doubles = Array(100 :: 100 ::  100 :: ⊥)((1 to 1000000).map(_.toDouble))
  )

foo.save(Path("foo.1m"))
```

yields:

```
zarr/foo.1m
├── [         24]  .zgroup
├── [        352]  doubles
│   ├── [        367]  .zarray
│   ├── [      10858]  0.0.0
│   ├── [       9814]  1.0.0
│   ├── [      10311]  2.0.0
│   ├── [       9552]  3.0.0
│   ├── [      10013]  4.0.0
│   ├── [       9349]  5.0.0
│   ├── [       9358]  6.0.0
│   └── [       7964]  7.0.0
└── [        224]  ints
    ├── [        345]  .zarray
    ├── [       9436]  0.0
    ├── [       9466]  1.0
    ├── [       9490]  2.0
    └── [       8736]  3.0
```

Chunk-sizes can also be passed explicitly to the `Array` constructor:

```scala
Array(
  1000 :: 1000 :: ⊥,
   500 ::  100 :: ⊥
)(
  1 to 1000000
)
```

Random-accesses can be performed like:

```scala
foo.ints(123 :: 456 :: ⊥)
// 123457
```

"Folds" also work:

```scala
foo.doubles.foldLeft(0.0)(_ + _)
// 5.000005E11
```

[`cats.Foldable`] syntax works, but requires the `.t` accessor:

```scala
foo.ints.t.toList
// List(1, 2, 3, … 999999, 1000000)
```

More sophisticated traversals and manipulations are still TODO.

[Zarr]: https://zarr.readthedocs.io/en/stable/
[`cats.Foldable`]: https://typelevel.org/cats/typeclasses/foldable.html
