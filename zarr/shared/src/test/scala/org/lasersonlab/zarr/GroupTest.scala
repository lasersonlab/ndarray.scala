package org.lasersonlab.zarr

import hammerlab.path._
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Compressor.Blosc.CName.lz4
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Order.C

class GroupTest
  extends hammerlab.Suite {
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
      Metadata(
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
        fill_value = Var.empty,
        zarr_format = `2`
      )
    )
  }
}
