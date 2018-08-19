package org.lasersonlab.zarr.untyped

import hammerlab.path._
import io.circe.Json
import org.lasersonlab.anndata.loom.Var
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Compressor.Blosc.CName.lz4
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr.{ Suite, untyped }

class GroupTest
  extends Suite {
  test("dir") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.ad.32m.zarr")

    val group @
      Group(
        arrays,
        groups,
        attrs
      ) =
      Group(path)
        .fold(
          fail(_),
          identity
        )

    arrays.keys should be(Set("X", "obs", "var"))
    groups.keys should be(Set("uns"))

    ==(attrs, None)

    val `var` = group.array('var)
    `var`.shape should be(27998 :: Nil)
    `var`.chunks should be(27998 :: Nil)

    `var`.metadata should be(
      untyped.Metadata(
        shape = Seq(27998),
        chunks = Seq(27998),
        dtype = Var.dtype,
        compressor =
          Blosc(
            cname = lz4,
            clevel = 5,
            shuffle = 1,
            blocksize = 0
          ),
        order = C,
        fill_value =
          Json.fromString(
            // 68 0-bytes, base64-encoded
            "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
          ),
        zarr_format = `2`
      )
    )
  }
}
