package org.lasersonlab.zarr.untyped

import java.nio.ByteBuffer

import hammerlab.path._
import org.lasersonlab.zarr.|

import scala.collection.mutable

trait Array {
  type T
  def metadata: Metadata.Aux[T]
  def apply(idxs: Int*): T
}

object Array {
  type Aux[_T] = Array { type T = _T }

  implicit def unwrap[T](a: Aux[T]): Metadata.Aux[T] = a.metadata

  def apply(dir: Path): Exception | Array =
    for {
      _metadata ← Metadata(dir)
    } yield
      new Array {

        val metadata = _metadata

        type T = _metadata.dtype.T

        val      shape = metadata.shape
        val chunkShape = metadata.chunks

        private val _chunks = mutable.Map[Seq[Int], ByteBuffer]()
        def chunk(idx: Seq[Int]): ByteBuffer =
          _chunks.getOrElseUpdate(
            idx,
            ByteBuffer.wrap(dir / idx.mkString(".") readBytes)
          )

        require(
          shape.size == chunkShape.size,
          s"Shape dimensions (${shape.size}: ${shape.mkString(",")}) don't match chunk dimensions (${chunkShape.size}: ${chunkShape.mkString(",")})"
        )

        val N = chunkShape.size

        val total :: products =
          chunkShape
            .scanRight(1)(_ * _)
            .toList

        def apply(idxs: Int*): T = {
          if (idxs.size != N)
            throw new IllegalArgumentException(
              s"Expected $N dimensions, found ${idxs.size}: ${idxs.mkString(",")}"
            )

          var idx = 0
          var sum = 0
          while (idx < N) {
            sum += (idxs(idx) % chunkShape(idx)) * products(idx)
            idx += 1
          }

          metadata.dtype(chunk(idxs), sum)
        }
      }
}
