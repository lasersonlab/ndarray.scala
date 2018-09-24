package org.lasersonlab.convert

import hammerlab.path._
import org.hammerlab.test.resources.File
import org.lasersonlab.zarr
import org.lasersonlab.zarr.{ Group, HasGetOps }
import org.lasersonlab.zarr.io.Load

class ConvertTest
  extends hammerlab.test.Suite
     with HasGetOps
     with Load.syntax
     with zarr.cmp.all {

  // TODO: move to test-utils
  def resource(name: String): Path = File(name).path

  test("hdf5 conversion") {
    val hdf5 = resource("hgmm_100_raw_gene_bc_matrices_h5.h5")

    val  `2m-path` = tmpDir()
    val `64m-path` = tmpDir()

    Main.main(            hdf5, `64m-path`)
    Main.main("-c", "2m", hdf5,  `2m-path`)

    val  `2m` =  `2m-path`.load[Group[Int]].get
    val `64m` = `64m-path`.load[Group[Int]].get

    {
      import dimensions.ignoreChunks
      ==(`2m`, `64m`)
    }

    val  `2m-expected` = resource("hgmm_100_raw_gene_bc_matrices.10x.2m.zarr" ).load[Group[Int]].get
    val `64m-expected` = resource("hgmm_100_raw_gene_bc_matrices.10x.64m.zarr").load[Group[Int]].get

    ==( `2m`,  `2m-expected`)
    ==(`64m`, `64m-expected`)
  }
}
