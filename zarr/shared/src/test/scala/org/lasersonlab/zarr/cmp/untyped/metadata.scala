package org.lasersonlab.zarr.cmp.untyped

import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.untyped.Metadata
import shapeless._

object metadata {

  trait cmp {
    object metadata {
      // import this to allow metadata to have different chunk-size fields when comparing arrays
      implicit val ignoreChunks = IgnoreChunks.Yes
    }
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

  sealed trait IgnoreChunks
  object IgnoreChunks {
    // Default: don't ignore chunk size when testing equality
    implicit object  No extends IgnoreChunks
             object Yes extends IgnoreChunks
  }
}
