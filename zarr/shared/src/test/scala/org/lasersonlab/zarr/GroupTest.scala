package org.lasersonlab.zarr

import hammerlab.path._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.untyped.Struct

class GroupTest
  extends Suite
    with zarr.cmp.all {
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

    val `var`  = group[Struct]('var)
    val `var2` = group ! 'var

    ==(`var`.shape, Dimension(27998) :: Nil)

    val datatype =
      DataType.untyped.Struct(
        List(
          StructEntry(    "index",   long    ),
          StructEntry("Accession", string(18)),
          StructEntry(     "Gene",  short    ),
          StructEntry(   "_LogCV", double    ),
          StructEntry( "_LogMean", double    ),
          StructEntry("_Selected",   long    ),
          StructEntry(   "_Total", double    ),
          StructEntry(   "_Valid",   long    )
        )
      )

    val expected =
      zarr.Metadata(
        shape = List(Dimension(27998)),
        dtype = datatype,
        fill_value =
          Struct(
            Map(
                  "index" → 0L,
              "Accession" → "",
                   "Gene" → 0.toShort,
                 "_LogCV" → 0.0,
               "_LogMean" → 0.0,
              "_Selected" → 0L,
                 "_Total" → 0.0,
                 "_Valid" → 0L
            )
          )
      )

    ==(
      `var`.metadata,
      expected
    )

    ==(
      `var2`
        .metadata
        .as[Struct],  // `var2` was accessed via the "untyped" group.array method
      expected
    )
  }

  test("save") {
    // TODO
  }
}
