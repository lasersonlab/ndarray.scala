package org.lasersonlab.zarr.cmp.untyped

import cats.Traverse
import cats.implicits._
import hammerlab.either._
import org.lasersonlab.test.Cmp
import org.lasersonlab.zarr.cmp.untyped.array.ElemsDiff.{ Index, Sizes }
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.zarr.{ Array, Attrs, Dimension, Metadata }
import shapeless.the

object array {

  val _attrs = the[Cmp[Option[Attrs]]]

  sealed trait ElemsDiff
  object ElemsDiff {
    case class Sizes(l: Int, r: Int)         extends ElemsDiff
    case class Index(i: Int, d: Any) extends ElemsDiff
  }

  trait cmp {

    implicit def arrayIdxsCmp[Idx: Idx.T](
      implicit
      dim: Cmp[Dimension[Idx]]
    ):
      Cmp[
        Array.*?[Idx]
      ] =
      arrayShapedCmp[List, Idx]

    import lasersonlab.{ zarr ⇒ z }

    implicit def arrayCmp[
      Shape[_]: Traverse,
      T
    ](
      implicit
       dim: Cmp[Shape[Dimension[Int]]],
      elem: Cmp[T]
    ):
    Cmp[
      z.Array[
        Shape,
        T
      ]
    ]
    = {
      type Arr = z.Array[Shape, T]
      Cmp {
        (l, r) ⇒
          val _metadata = the[Cmp[Metadata[Shape, Int, T]]]

          type Diff =
            _metadata.Diff |
               _attrs.Diff |
                 ElemsDiff

          _metadata(l.metadata, r.metadata)
            .map(d ⇒ L(L(d)))
            .orElse {
              _attrs(l.attrs, r.attrs)
                .map(d ⇒ L(R(d)))
                .orElse {
                  the[Cmp[Seq[T]]]
                    .apply(l.t.toList, r.t.toList)  // TODO: make Foldable syntax work without .t here
                    .map { R(_) }
                }
            }
      }
    }

    def arrayShapedCmp[
      Shape[_]: Traverse,
        Idx   : Idx.T
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
      Cmp {
        (l, r) ⇒
          metadata.cmp.baseCmp[Shape, Idx]
            .apply(l.metadata, r.metadata)
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
