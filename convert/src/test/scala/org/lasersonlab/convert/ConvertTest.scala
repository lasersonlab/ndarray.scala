package org.lasersonlab.convert

import cats.implicits._
import lasersonlab.zarr.{ Array ⇒ _, _ }  // work-around for https://github.com/lihaoyi/utest/pull/186/files#r247263418
import org.hammerlab.cli.base.app.Arg
import org.lasersonlab.test.future.Assert
import org.lasersonlab.uri.Local
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Dimension
import org.lasersonlab.zarr.io.Load
import utest._

object ConvertTest
  extends lasersonlab.Suite
     with Load.syntax
     with zarr.cmp.all
     with Assert.syntax
{
  println(s"file: $file, cwd: ${Local.cwd}")

  val tests = Tests {
    'hdf5 - {
      import lasersonlab.zarr.Array
      val hdf5 = resource("hgmm_100_raw_gene_bc_matrices_h5.h5")

      val  `2m-path` = tmpPath()
      val `64m-path` = tmpPath()

      ()
      implicit def pathToArg(path: Path): Arg = Arg(path.toString)

      // TODO: capture + verify the stdout from these applications (which contains verbose printlns of the groups'
      //  metadata
      Main.main(            "-t", "2", hdf5, `64m-path`)
      Main.main("-c", "2m", "-t", "2", hdf5,  `2m-path`)

      for {
         m2 ←  `2m-path`.load[Group]
        m64 ← `64m-path`.load[Group]
        barcodes = (m2 / 'hg19).→[String]('barcodes)
        _ ←
           ==(
             barcodes.shape,
             List(
               Dimension.int(
                 737280,
                 116508
               )
             )
           )
        _ ← {
          import dimensions.ignoreChunks
          ==(m2, m64)
        }

         expectedM2 ← resource("hgmm_100_raw_gene_bc_matrices.10x.2m.zarr" ).load[Group]
        expectedM64 ← resource("hgmm_100_raw_gene_bc_matrices.10x.64m.zarr").load[Group]

        _ ← ==( m2,  expectedM2)
        _ ← ==(m64, expectedM64)

      } yield
        ()
    }
  }
}
