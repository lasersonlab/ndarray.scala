package org.lasersonlab.convert

import cats.implicits._
import lasersonlab.zarr._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.cmp.Cmp
import org.lasersonlab.zarr.io.Load
import org.lasersonlab.zarr.{ Dimension, HasGetOps }

class ConvertTest
  extends hammerlab.test.Suite
     with HasGetOps
     with Load.syntax
     with zarr.cmp.all
     with Cmp.syntax {

  test("hdf5 conversion") {
    val hdf5 = resource("hgmm_100_raw_gene_bc_matrices_h5.h5")

    val  `2m-path` = tmpDir()
    val `64m-path` = tmpDir()

    Main.main(            hdf5, `64m-path`)
    Main.main("-c", "2m", hdf5,  `2m-path`)

    val  `2m` =  `2m-path`.load[Group] !
    val `64m` = `64m-path`.load[Group] !

    val barcodes = (`2m` / 'hg19).→[String]('barcodes)
    ==(
      barcodes.shape,
      List(
        Dimension.int(
          737280,
          116508
        )
      )
    )

    {
      import dimensions.ignoreChunks
      eqv(`2m`, `64m`)
    }

    val  `2m-expected` = resource("hgmm_100_raw_gene_bc_matrices.10x.2m.zarr" ).load[Group] !
    val `64m-expected` = resource("hgmm_100_raw_gene_bc_matrices.10x.64m.zarr").load[Group] !

    eqv( `2m`,  `2m-expected`)
    eqv(`64m`, `64m-expected`)
  }
}
