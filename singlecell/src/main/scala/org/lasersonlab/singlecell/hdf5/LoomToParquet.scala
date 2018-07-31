package org.lasersonlab.singlecell.hdf5

import hammerlab.path._
import com.tom_e_white.hdf5_java_cloud.ArrayUtils
import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import org.apache.spark.mllib.linalg.{ Vector, Vectors }
import org.apache.spark.sql.{ Row, SaveMode, SparkSession }
import org.apache.spark.sql.types._
import ucar.nc2.NetcdfFile

object LoomToParquet {
  def apply(input: Path, output: Path, totalShards: Int = 500)(implicit spark: SparkSession): Unit = {

    implicit val sc = spark.sparkContext

    val t0 = System.currentTimeMillis()

//    val input = "files/mca.loom" // change to "gs://..." for GCS
//    val output = "loom_parquet"
//    val totalShards = 500 // chunk size TODO: actually use something related to HDF5 chunk size

    // Read the k'th shard of the HDF5 file and return a sequence of barcode-vector tuples. Each shard must fit in memory.
    def readShard(k: Int): Seq[Vector] = {
      val raf = new NioReadOnlyRandomAccessFile(input)
      val ncfile = NetcdfFile.open(raf, input.toString, null, null)
      val matrix = ncfile.findVariable("matrix")

      val numRows = matrix.getShape(0)
      val numCols = matrix.getShape(1)

      // we are going to shard by cols since loom files are short and fat
      val start = k * numCols / (totalShards - 1)
      var end = 0
      if (k == (totalShards - 1)) end = numCols
      else end = (k + 1) * numCols / (totalShards - 1)

      // since it is chunked by column, slice by column, and then transpose
      val matrixData: Array[Array[Double]] = ArrayUtils.index(matrix, 1, start, end + 1)
        .transpose(0, 1)
        .copyToNDJavaArray
        .asInstanceOf[Array[Array[Double]]]

      matrixData.indices.map(i => {
        Vectors.dense(matrixData(i))
      })
    }

    val actualShards = totalShards // change this to test on a subset
    val shardIndexes = sc.parallelize(0 until actualShards, totalShards)
    val rows = shardIndexes.flatMap(readShard).map { vec ⇒ Row(vec.toArray) }

    val schema = StructType(
      StructField("quant", ArrayType(DoubleType, false), false) :: Nil
    )

    val df = spark.createDataFrame(rows, schema)
    df.write.mode(SaveMode.Overwrite).parquet(output.toString)

    val t1 = System.currentTimeMillis()

    println("Elapsed time: " + ((t1 - t0) / 1000) + "s")

    val df2 = spark.read.parquet("files/loom_parquet")
    df2.count
  }
}
