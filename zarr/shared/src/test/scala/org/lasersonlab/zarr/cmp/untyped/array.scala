package org.lasersonlab.zarr.cmp.untyped

import hammerlab.either._
import hammerlab.option._
import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.cmp.untyped.array.ElemsDiff.{ Index, Sizes }
import org.lasersonlab.zarr.cmp.untyped.array.metadata.IgnoreChunks
import org.lasersonlab.zarr.untyped.Metadata
import org.lasersonlab.zarr.{ Array, Attrs }
import shapeless._

object array {

  trait metadata {
    implicit def metadataCmp[S: Cmp](
      implicit
      i: IgnoreChunks
    ):
      Cmp[Metadata.S[S]] = {
      i match {
        case IgnoreChunks. No ⇒
          Cmp.by {
            m ⇒
              m.      shape ::
              m.     chunks ::
              m.      dtype ::
              m. compressor ::
              m.      order ::
              //m.fill_value ::      TODO: restore fill_value cmp
              m.zarr_format ::
              m.    filters ::
                            HNil
          }
        case IgnoreChunks.Yes ⇒
          Cmp.by {
            m ⇒
              m.      shape ::
              m.      dtype ::
              m. compressor ::
              m.      order ::
              //m.fill_value ::
              m.zarr_format ::
              m.    filters ::
                            HNil
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

    implicit def arrayCmp[S](implicit i: IgnoreChunks, s: Cmp[S]): Cmp[Array.SU[S]] =
      new Cmp[Array.SU[S]] {

        val _metadata = metadataCmp(s, i)

        type Diff =
          _metadata.Diff |
             _attrs.Diff |
               ElemsDiff

        def cmp(l: Array.SU[S], r: Array.SU[S]): Option[Diff] =
          _metadata(l.metadata, r.metadata)
            .map(d ⇒ L(L(d)))
            .orElse {
              _attrs(l.attrs, r.attrs)
                .map(d ⇒ L(R(d)))
                .orElse {
                  var i = 0
                  var diff: Option[ElemsDiff] = None
                  // TODO: get cats syntax working here
                  val  left = l.traverseA.toList(l.chunks).iterator.flatMap { chunk ⇒ l.foldableChunk.toList(chunk).iterator }
                  val right = r.traverseA.toList(r.chunks).iterator.flatMap { chunk ⇒ r.foldableChunk.toList(chunk).iterator }
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
