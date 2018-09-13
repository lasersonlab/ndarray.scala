package org.lasersonlab.zarr

import hammerlab.path._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.FillValue.Null
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.untyped.Struct

class GroupTest
  extends Suite {
  test("load") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.ad.32m.zarr")

    val group @
      Group(
        arrays,
        groups,
        attrs,
        metadata
      ) =
      Group(path)
        .fold(
          fail(_),
          identity
        )

    ==(arrays.keySet, Set("X", "obs", "var"))
    ==(groups.keySet, Set("uns"))

    ==(attrs, None)
    ==(metadata, Group.Metadata(`2`))

    val `var` = group.array('var)

    ==(`var`.shape, 27998 :: Nil)

    `var`.metadata should be(
      zarr.Metadata[Struct, Seq[Int]](
         shape = Seq(27998),
        chunks = Seq(27998),
         dtype =
          struct(
            Vector(
              StructEntry(    "index",   long    ),
              StructEntry("Accession", string(18)),
              StructEntry(     "Gene",  short    ),
              StructEntry(   "_LogCV", double    ),
              StructEntry( "_LogMean", double    ),
              StructEntry("_Selected",   long    ),
              StructEntry(   "_Total", double    ),
              StructEntry(   "_Valid",   long    )
            )
          ),
        fill_value = Null  // TODO: fill values are all null atm
//          Struct(
//            Map(
//                  "index" → 0L,
//              "Accession" → "",
//                   "Gene" → 0.toShort,
//                 "_LogCV" → 0.0,
//               "_LogMean" → 0.0,
//              "_Selected" → 0L,
//                 "_Total" → 0.0,
//                 "_Valid" → 0L
//            )
//          )
      )
    )
  }

  test("save") {
    // TODO
  }
}
