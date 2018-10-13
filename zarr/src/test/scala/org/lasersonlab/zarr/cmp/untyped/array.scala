package org.lasersonlab.zarr.cmp.untyped

import cats.implicits._
import hammerlab.either._
import hammerlab.option._
import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.cmp.untyped.array.ElemsDiff.{ Index, Sizes }
import org.lasersonlab.zarr.{ Array, Attrs, Dimension }
import shapeless.the

object array {

  val _attrs = the[Cmp[Opt[Attrs]]]

  sealed trait ElemsDiff
  object ElemsDiff {
    case class Sizes(l: Int, r: Int)         extends ElemsDiff
    case class Index(i: Int, d: Any) extends ElemsDiff
  }

  trait cmp {

    implicit def arrayIdxsCmp[Idx](
      implicit
      dim: Cmp[Dimension[Idx]]
    ):
      Cmp[
        Array.??[Idx]
      ] =
      arrayShapedCmp[List, Idx]

    import lasersonlab.{ zarr ⇒ z }

    implicit def arrayCmp[
      Shape[_],
      T
    ](
      implicit
      dim: Cmp[Shape[Dimension[Int]]],
      elem: Cmp[T]
    ):
    org.lasersonlab.zarr.Cmp[
      z.Array[
        Shape,
        T
      ]
    ]
    = {
      type Arr = z.Array[Shape, T]
      new org.lasersonlab.zarr.Cmp[Arr] {
        val _metadata = metadata.cmp.baseCmp[Shape, Int]

        type Diff =
          _metadata.Diff |
             _attrs.Diff |
               ElemsDiff

        def apply(l: Arr, r: Arr): Option[Diff] =
          _metadata(l.metadata, r.metadata)
            .map(d ⇒ L(L(d)))
            .orElse {
              _attrs(l.attrs, r.attrs)
                .map(d ⇒ L(R(d)))
                .orElse {
                  var i = 0
                  var diff: Option[ElemsDiff] = None
                  val  left = l.t.toList.iterator
                  val right = r.t.toList.iterator
                  while (left.hasNext && right.hasNext && diff.isEmpty) {
                    val l =  left.next
                    val r = right.next
                    elem(l, r)
                      .foreach {
                        d ⇒
                          diff = Some(Index(i, d))
                      }
                    i += 1
                  }

                  diff
                    .orElse {
                      if (left.hasNext || right.hasNext)
                        Some(
                          Sizes(
                            i + left.size,
                            i + right.size
                          )
                        )
                      else
                        None
                    }
                    .map { R(_) }
                }
            }
      }
    }

      def arrayShapedCmp[
      Shape[_],
      Idx
    ](
      implicit
      dim: Cmp[Shape[Dimension[Idx]]]
    ):
      Cmp[
        Array.?[
          Shape,
          Idx
        ]
      ]
    = {
      type Arr = Array.?[Shape, Idx]
      new Cmp[Arr] {
        val _metadata = metadata.cmp.baseCmp[Shape, Idx]

        type Diff =
          _metadata.Diff |
             _attrs.Diff |
               ElemsDiff

        def cmp(l: Arr, r: Arr): Option[Diff] =
          _metadata(l.metadata, r.metadata)
            .map(d ⇒ L(L(d)))
            .orElse {
              _attrs(l.attrs, r.attrs)
                .map(d ⇒ L(R(d)))
                .orElse {
                  var i = 0
                  var diff: Option[ElemsDiff] = None
                  val  left = l.t.toList.iterator
                  val right = r.t.toList.iterator
                  while (left.hasNext && right.hasNext && diff.isEmpty) {
                    val l =  left.next
                    val r = right.next
                    if (l != r)  // TODO: can we thread a custom comparator down here?
                      diff = Some(Index(i, (l, r)))
                    i += 1
                  }
                  diff
                    .orElse {
                      if (left.hasNext || right.hasNext)
                        Some(
                          Sizes(
                            i + left.size,
                            i + right.size
                          )
                        )
                      else
                        None
                    }
                    .map { R(_) }
                }
            }
      }
    }
  }
  object cmp extends cmp
}
