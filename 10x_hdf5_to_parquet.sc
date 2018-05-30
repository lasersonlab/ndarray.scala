// ~/sw/spark-2.2.1-bin-hadoop2.7/bin/spark-shell --master local[2] --jars ~/.m2/repository/com/tom_e_white/hdf5-java-cloud/0.0.1-SNAPSHOT/hdf5-java-cloud-0.0.1-SNAPSHOT.jar

import hammerlab.path._
import com.tom_e_white.hdf5_java_cloud.ArrayUtils
import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import org.apache.spark.SparkContext
import org.apache.spark.mllib.linalg.{ SparseVector, Vector, Vectors }
import org.apache.spark.sql.{ Row, SaveMode, SparkSession }
import org.apache.spark.sql.types._
import ucar.nc2.NetcdfFile

val t0 = System.currentTimeMillis()

val file = "files/1M_neurons_filtered_gene_bc_matrices_h5.h5" // change to "gs://..." for GCS
val output = "10x_parquet"
val totalShards = 320

def sc: SparkContext
def spark: SparkSession

// Read the k'th shard of the HDF5 file and return a sequence of barcode-vector tuples. Each shard must fit in memory.
def readShard(k: Int): Seq[(String, Vector)] = {
  val location = file
  val path = Path(location)
  val raf = new NioReadOnlyRandomAccessFile(path)
  val ncfile = NetcdfFile.open(raf, location, null, null)
  val indptr = ncfile.findVariable("/mm10/indptr")
  val indices = ncfile.findVariable("/mm10/indices")
  val data = ncfile.findVariable("/mm10/data")
  val barcodes = ncfile.findVariable("/mm10/barcodes")
  val shape = ncfile.findVariable("/mm10/shape")

  val numFeatures = shape.read.getInt(0)

  val numRows = barcodes.getShape(0)
  val start = k * numRows / (totalShards - 1)
  var end = 0
  if (k == (totalShards - 1)) end = numRows
  else end = (k + 1) * numRows / (totalShards - 1)

  val barcodeData: Array[String] = ArrayUtils.index(barcodes, start, end + 1)
    .copyToNDJavaArray().asInstanceOf[Array[Array[Char]]]
    .map(x => x.mkString)
  val indptrData: Array[Long] = ArrayUtils.index(indptr, start, end + 1).getStorage.asInstanceOf[Array[Long]]
  val firstIndptr: Long = indptrData(0)
  val lastIndptr: Long = indptrData.last
  if (firstIndptr == lastIndptr) {
    return Seq()
  }
  val indicesData: Array[Long] = ArrayUtils.index(indices, firstIndptr, lastIndptr).getStorage.asInstanceOf[Array[Long]]
  val dataData: Array[Int] = ArrayUtils.index(data, firstIndptr, lastIndptr).getStorage.asInstanceOf[Array[Int]]

  (0 until end - start).map {
    i ⇒
      val barcode = barcodeData(i)
      val indicesSlice = indicesData.slice((indptrData(i) - firstIndptr).toInt, (indptrData(i + 1) - firstIndptr).toInt)
      val dataSlice = dataData.slice((indptrData(i) - firstIndptr).toInt, (indptrData(i + 1) - firstIndptr).toInt)
      val indexDataPairs =
        indicesSlice
          .zip(dataSlice)
          .map {
            case (k: Long, v: Int) ⇒
              (k.toInt, v.toDouble)  // Vector is (Int, Double)
          }
    val vec = Vectors.sparse(numFeatures, indexDataPairs)
      (barcode, vec)
  }
}

val actualShards = totalShards  // change this to test on a subset
val shardIndexes = sc.parallelize(0 until actualShards, totalShards)
val rows =
  shardIndexes
    .flatMap(readShard)
    .map {
      case (id, vec) ⇒
        Row(
          id,
          vec
            .asInstanceOf[SparseVector]
            .indices,
          vec
            .asInstanceOf[SparseVector]
            .values
        )
    }

val schema = StructType(
  StructField(   "id", StringType                    , false) ::
  StructField(  "idx",  ArrayType(IntegerType, false), false) ::
  StructField("quant",  ArrayType( DoubleType, false), false) :: Nil
)

val df = spark.createDataFrame(rows, schema)
df.write.mode(SaveMode.Overwrite).parquet(output)

val t1 = System.currentTimeMillis()

println("Elapsed time: " + ((t1 - t0) / 1000) + "s")
