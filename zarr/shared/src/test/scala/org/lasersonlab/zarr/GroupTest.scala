package org.lasersonlab.zarr

import hammerlab.path._
import org.hammerlab.cmp.CanEq.dsl
import org.hammerlab.cmp.Show
import org.hammerlab.test.Cmp
import org.lasersonlab.zarr
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

    val `var`  = group[Struct]('var)
    val `var2` = group.array('var)

    ==(`var`.shape, Dimension(27998) :: Nil)

    val datatype =
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
      )

    import org.lasersonlab.zarr.cmp.cmpStruct

    val expected =
      zarr.Metadata[Struct, List, Int](
        shape = List(Dimension(27998)),
        dtype = datatype,
        fill_value =
          // TODO: fill untyped values are null atm; fix is to move fill-value handling into datatype
          FillValue.Null
          /*Struct(
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
          )*/
      )

    ==(
      `var`.metadata,
       expected
    )

    {
      // use a custom comparator for the untyped version
      import zarr.cmp.metadataCmp

      val cmp = metadataCmp[List]

      !![Cmp[untyped.Metadata.S[List, Int]]]

      ==[
        untyped.Metadata.S[List, Int],
        untyped.Metadata.S[List, Int],
        cmp.Diff
      ](
        `var2`.metadata: untyped.Metadata.S[List, Int],
        expected: untyped.Metadata.S[List, Int]
      )(
        cmp,
        Show.showAny[cmp.Diff]
      )
    }
  }

  test("save") {
    // TODO
  }
}
