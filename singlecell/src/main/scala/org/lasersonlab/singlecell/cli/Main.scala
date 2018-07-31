package org.lasersonlab.singlecell.cli

import hammerlab.path._
import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession
import org.lasersonlab.singlecell.hdf5.Parquet

object Main {
   def main(args: Array[String]): Unit = {
     implicit val conf = new SparkConf
     implicit val spark =
       SparkSession
         .builder()
         .appName("hdf5-to-parquet")
         .config(conf)
         .getOrCreate()

     Parquet(
       input = Path("files/1M_neurons_filtered_gene_bc_matrices_h5.h5"),
       partitions = 320,
       output = Path("10x_parquet")
     )
   }
}
