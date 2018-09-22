package org.lasersonlab.zarr.cmp.untyped

import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.Dimension
import org.lasersonlab.zarr.untyped.Metadata
import shapeless._

object metadata {

  trait cmp {
    object metadata {
      // import this to allow metadata to have different chunk-size fields when comparing arrays
      implicit val ignoreChunks = IgnoreChunks.Yes
    }
    implicit def metadataCmp[S[_]](
      implicit
      i: IgnoreChunks,
      scmp: Cmp[S[Dimension[Int]]]
    ):
      Cmp[Metadata.S[S, Int]] =
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

  sealed trait IgnoreChunks {
    def cmp[S[_]]: Cmp[Metadata.S[S, Int]]
  }
  object IgnoreChunks {
    // Default: don't ignore chunk size when testing equality
    implicit object No
      extends IgnoreChunks {
      implicit def scmp[S[_]]: Cmp[S[Dimension[Int]]] = ???
      def cmp[S[_]]: Cmp[Metadata.S[S, Int]] =
        Cmp.by {
          m ⇒
            m.      shape ::  // TODO: ignore chunks
            m.      dtype ::
            m. compressor ::
            m.      order ::
            //m.fill_value ::      TODO: restore fill_value cmp
            m.zarr_format ::
            m.    filters ::
            HNil
        }

    }
    object Yes
      extends IgnoreChunks {
      implicit def scmp[S[_]]: Cmp[S[Dimension[Int]]] = ???
      def cmp[S[_]]: Cmp[Metadata.S[S, Int]] =
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
