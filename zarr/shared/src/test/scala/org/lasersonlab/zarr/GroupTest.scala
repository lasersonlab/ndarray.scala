package org.lasersonlab.zarr

import hammerlab.path._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.dtype.DataType.{ untyped, _ }
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
    val `var2` = group.array('var)

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

    val actual = `var2`.metadata

    // auto-Cmp-derivation doesn't work  for `var2` because its element-type is unknown at compile-time
    // here we verify its the fields individually
    ==( actual       shape, expected       shape )
    ==( actual  compressor, expected  compressor )
    ==( actual       order, expected       order )
    ==( actual zarr_format, expected zarr_format )
    ==( actual     filters, expected     filters )

    // drop opaque dependent-type from the left side
    ==( actual dtype: DataType, expected dtype )

    // TODO: fold fill-values into datatype
    ==( actual. fill_value.asInstanceOf[FillValue[Struct]], expected fill_value )

    // casting to a fully-typed representation allows normal checking to succeed; this is redundant with the checks
    // above, but both are included for demonstration purposes
    ==(
      actual.asInstanceOf[Metadata[List, Int, Struct]],
      expected
    )
  }

  test("save") {
    // TODO
  }
}
