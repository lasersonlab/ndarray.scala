package org.lasersonlab.singlecell.hdf5

import org.apache.spark.SparkContext
import org.apache.spark.mllib.linalg.{ Vector, Vectors }

import scala.math.log1p

// Seurat
// > mat <- matrix(data = rbinom(n = 30, size = 5, prob = 0.2), nrow = 6)
// > mat
//      [,1] [,2] [,3] [,4] [,5]
// [1,]    2    0    1    1    0
// [2,]    2    0    2    0    0
// [3,]    1    0    1    2    0
// [4,]    0    2    1    1    0
// [5,]    1    1    3    1    2
// [6,]    0    1    1    0    2
// > mat_norm <- LogNormalize(data = mat)
// Performing log-normalization
// 0%   10   20   30   40   50   60   70   80   90   100%
// [----|----|----|----|----|----|----|----|----|----|
// **************************************************|
// > mat_norm
// 6 x 5 sparse Matrix of class "dgCMatrix"
//
// [1,] 8.112028 .        7.014015 7.601402 .
// [2,] 8.112028 .        7.706713 .        .
// [3,] 7.419181 .        7.014015 8.294300 .
// [4,] .        8.517393 7.014015 7.601402 .
// [5,] 7.419181 7.824446 8.112028 7.601402 8.517393
// [6,] .        7.824446 7.014015 .        8.517393

object toy {
  def logNormalize(vec: Vector, scaleFactor: Double = 1e4): Vector = {
    val sum = vec.toArray.sum
    Vectors.dense(vec.toArray.map(v => log1p(v / sum * scaleFactor)))
  }

  val data = Array(
    Vectors.dense(Array(2.0, 2.0, 1.0, 0.0, 1.0, 0.0)),
    Vectors.dense(Array(0.0, 0.0, 0.0, 2.0, 1.0, 1.0))
  )

  def apply(implicit sc: SparkContext) = {
    val rows = sc.parallelize(data)
    var normalized = rows.map(logNormalize(_))

    rows.foreach(println(_))
    normalized.foreach(println(_))
  }
}
