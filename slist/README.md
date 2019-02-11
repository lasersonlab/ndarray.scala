# slist

Type-level lists with a single element type; named after "same-element [`HList`](https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-1.2.4#heterogenous-lists)s".


<table>
<thead>
<tr>
  <td rowspan="2" colspan="2">
  </td>
  <td colspan="2" align="center">
    <b>List length</b>
  </td>
</tr>
<tr>
  <td align="center">
    <b>Type-level</b>
  </td>
  <td align="center">
    <b>Value-level</b>
  </td>
</tr>
</thead>
<tbody>
<tr>
  <td rowspan="2">
    <b>Element types</b>
  </td>
  <td>
    <b>Homogeneous</b>
  </td>
  <td align="center">
    SLists: <br/><code>`2`[T]</code>,  <code>`3`[T]</code>, <br/> etc. 😍
  </td>
  <td align="center">
    <code>List[T]</code> 🤷🏼‍♂️
  </td>
</tr>
<tr>
  <td>
    <b>Heterogeneous</b>
  </td>
  <td align="center">
    <code>HList</code> 👍🏼
  </td>
  <td align="center">
    <code>List[Any]</code> 😱
  </td>
</tr>
</tbody>
</table>

The maximum length of an `SList` is currently <code>`9`</code> due to unresolved issues with automatic implicit derivation ([see discussion below](#Derivations)).

## Examples ##
(adapted from [SListTest](shared/src/test/scala/org/lasersonlab/slist/SListTest.scala))

### Enable `SList`s:

```scala
import lasersonlab.slist._
```

### Create `SList`s using a variant of `List` syntax:

```scala
val x1 = 111               :: ⊥  //                   `1`(222)
val x2 = 111 :: 222        :: ⊥  //          `2`(111, `1`(222))
val x3 = "a" :: "b" :: "c" :: ⊥  // `3`("a", `2`("b", `1`("c")))
```

Note that:
- `Nil` is replaced by `⊥` (["up tack"](https://www.fileformat.info/info/unicode/char/22a5/index.htm))
  - `⟘` (["large up tack"](http://www.fileformat.info/info/unicode/char/27d8/index.htm)) also works, as does the literal <code>`0`</code> (in backticks)
- the constructed types are numeric literals in back-ticks.

### Traversing

A [`cats.Traverse`](https://typelevel.org/cats/typeclasses/traverse.html) is provided for each length:
```scala
import cats.implicits._
```

#### Fold / Reduce

```scala
x2.reduce  //  333
x3.reduce  // "abc"
```

#### Map

```scala
val s2 = x2.map { _.toString }  // "111" :: "222" :: ⊥
```

#### Traverse / "zip" / "transpose"

Let's build a `3`-length list of `2`-length lists:

```scala
val mod10  = x2.map { _ %  10 }  // 1 :: 2 :: ⊥
val mod100 = x2.map { _ % 100 }

val `3x2` = x2 :: mod100 :: mod10 :: ⊥  // `3`[`2`[T]]
```

`Traverse.sequence` "transposes" this <code>`3`[`2`[T]]</code> into a <code>`2`[`3`[T]]</code> (shown here with an destructuring assignment):

```
val ones :: twos :: ⊥ = `3x2` sequence

ones  // 111 :: 11 :: 1 :: ⊥
twos  // 222 :: 22 :: 2 :: ⊥
```

## Derivations

Unfortunately, I've not been able to get implicit derivations of type-class instances to work for `SList`s in a nice inductive fashion like you'd hope (while preserving other commonsense syntax); some details are in [this comment in `SList.scala`](shared/src/main/scala/org/lasersonlab/slist/SList.scala#L60-76).

The best you can do is define the inductive step and call it yourself:

```scala
// Some higher-kinded type-class (e.g. cats.Functor) that we'd like to auto-derive for all SLists
trait MyHKT[F[_]]

object MyHKT {
  import lasersonlab.slist._

  // Inductive step: given an instance for a "Tail"-type, derive an instance for its Cons successor
  implicit def induct[Tail[_]]: MyHKT](implicit cons: Cons[Tail]): MyHKT[cons.Out] = ???

  // Base-case: define type-class for bottom type ⟘
  implicit val hkt0 : HKT[`0`] = ???

  // This part wouldn't be necessary, ideally; manually unroll instances:
  implicit val hkt1 : HKT[`1`] = induct[`0`]
  implicit val hkt2 : HKT[`2`] = induct[`1`]
  implicit val hkt3 : HKT[`3`] = induct[`2`]
  implicit val hkt4 : HKT[`4`] = induct[`3`]
  implicit val hkt5 : HKT[`5`] = induct[`4`]
  implicit val hkt6 : HKT[`6`] = induct[`5`]
  implicit val hkt7 : HKT[`7`] = induct[`6`]
  implicit val hkt8 : HKT[`8`] = induct[`7`]
  implicit val hkt9 : HKT[`9`] = induct[`8`]
}
```

Note: `Cons` is a type-function from each `SList` type to its (one-larger) successor; instances are provided for all `SList`s (except <code>`9`</code>, the current terminus).

## See also

The [Zarr](../zarr) module demonstrates using `SList`s as indices into multidimensional arrays, so that the number of dimensions is enforced by the type-system.
