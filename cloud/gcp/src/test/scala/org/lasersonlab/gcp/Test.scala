package org.lasersonlab.gcp

import hammerlab.path._
import lasersonlab.gcp

class Test
  extends hammerlab.Suite {
  test("list gcs bucket") {
    val path = Path("gs://ll-sc-data")
    gcp.userProject("hca-scale")
    println(path.list.mkString("\n"))
  }
}
