package org.lasersonlab.convert

import cats.{ Eval, Foldable }
import ucar.ma2.IndexIterator

/**
 * Zarr "chunk" implementation that wraps a section of a NetCDF [[ucar.nc2.Variable]]
 *
 * @param variable underlying dataset
 * @param start chunk "origin"
 * @param shape chunk shape
 * @param sectionShape the shape of one element in the chunk; this is generally an array with `rank` copies of the
 *                     literal `1`, but in the case of converting char-arrays to fixed-length strings,
 * @param next callback for pulling the next element from an [[IndexIterator]]
 * @tparam T constituent element type
 */
case class Chunk[T](
  variable: ucar.nc2.Variable,
  start: Int,
  shape: Array[Int],
  sectionShape: Array[Int]
)(
  val next: IndexIterator ⇒ T
) {
  val rank = shape.length
  require(
    sectionShape.length <= rank + 1,
    s"Section shape (${sectionShape.mkString(",")}) is ${sectionShape.length - rank} longer than rank $rank (row: $start, shape ${shape.mkString(",")})"
  )
  def apply(idxs: Seq[Int]): T = {
    require(
      idxs.size == rank,
      s"Index of size ${idxs.size} (${idxs.mkString(",")}) for array of rank $rank"
    )

    val builder = Array.newBuilder[Int]
    for {
      i ← 0 until rank
       idx =  idxs(i)
      size = shape(i)
    } {
      val idx =
        if (i == 0)
          idxs.head + start
        else
          idxs(i)
      if (idx >= size)
        throw new IllegalArgumentException(
          s"Index $idx is larger than chunk-size $size (row $start; shape ${shape.mkString(",")}"
        )

      builder += idx
    }

    if (sectionShape.length == rank + 1)
      builder += 0

    val origin = builder.result()

    next(
      variable
        .read(origin, sectionShape)
        .getIndexIterator
    )
  }
}

object Chunk {
  implicit val foldable: Foldable[Chunk] =
    new Foldable[Chunk] {
      def foldLeft [A, B](fa: Chunk[A], b: B)(f: (B, A) ⇒ B): B = {
        import fa._
        val origin = Array.fill(sectionShape.length)(0)
        val shape =
          if (sectionShape.length == rank + 1)
            fa.shape :+ sectionShape.last
          else
            fa.shape
        origin(0) = start
        val data =
          variable
            .read(
              origin,
              shape
            )
            .getIndexIterator

        var ret = b
        while (data.hasNext) {
          ret = f(ret, next(data))
        }
        ret
      }
      def foldRight[A, B](fa: Chunk[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
    }
}
