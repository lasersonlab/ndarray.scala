package org.lasersonlab.zarr.cmp.untyped

import hammerlab.either._
import hammerlab.option._
import org.hammerlab.cmp.CanEq
import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.cmp.untyped.array.ElemsDiff.{ Index, Sizes }
import org.lasersonlab.zarr.cmp.untyped.array.metadata.IgnoreChunks
import org.lasersonlab.zarr.untyped.Metadata
import org.lasersonlab.zarr.{ Attrs, Filter, untyped }
import shapeless.ops.hlist.Drop
import shapeless.{ Generic, the }
import shapeless.nat._2

object array {

  trait metadata {
    implicit def metadataCmp(implicit ignoreChunks: IgnoreChunks): Cmp[Metadata] = {
      ignoreChunks match {
        case IgnoreChunks. No ⇒ CanEq.cmpCaseClass
        case IgnoreChunks.Yes ⇒
          /**
           * drop the second field (chunk size) from the [[Generic]]/[[shapeless.HList]] representation of [[Metadata]]
           */
          val generic = the[Generic[Metadata]]
          val drop = the[Drop[generic.Repr, _2]]
          Cmp.by[
            drop.Out,
            Metadata
          ] {
            m ⇒
              drop(
                generic.to(
                  m
                )
              )
          }
      }
    }
  }
  object metadata {
    sealed trait IgnoreChunks
    object IgnoreChunks {
      // Default: don't ignore chunk size when testing equality
      implicit object  No extends IgnoreChunks
               object Yes extends IgnoreChunks
    }
  }

  val _attrs = the[Cmp[Opt[Attrs]]]

  sealed trait ElemsDiff
  object ElemsDiff {
    case class Sizes(l: Int, r: Int)         extends ElemsDiff
    case class Index(i: Int, l: Any, r: Any) extends ElemsDiff
  }

  trait cmp
    extends metadata {

    object metadata {
      // import this to allow metadata to have different chunk-size fields when comparing arrays
      implicit val ignoreChunks = IgnoreChunks.Yes
    }

    implicit def arrayCmp(implicit i: IgnoreChunks): Cmp[untyped.Array] =
      new Cmp[untyped.Array] {

        val _metadata = metadataCmp(i)

        type Diff =
          _metadata.Diff |
             _attrs.Diff |
               ElemsDiff

        def cmp(l: untyped.Array, r: untyped.Array): Option[Diff] =
          _metadata(l.metadata, r.metadata)
            .map(d ⇒ L(L(d)))
            .orElse {
              _attrs(l.attrs, r.attrs)
                .map(d ⇒ L(R(d)))
                .orElse {
                  var i = 0
                  var diff: Option[ElemsDiff] = None
                  val  left = l.elems
                  val right = r.elems
                  while (left.hasNext && right.hasNext && diff.isEmpty) {
                    val l =  left.next
                    val r = right.next
                    if (l != r)
                      diff = Some(Index(i, l, r))
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
  object cmp extends cmp
}
