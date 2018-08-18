package org.lasersonlab.zarr.untyped

import java.nio.ByteBuffer

import hammerlab.path._
import org.lasersonlab.zarr.Metadata.untyped
import org.lasersonlab.zarr.|

import scala.collection.mutable

trait Array {
  type T
  def metadata: untyped.Metadata
  def shape: Seq[Int] = metadata.shape
  def apply(idxs: Int*): T
}

object Array {
  type Aux[_T] = Array { type T = _T }

  def apply(dir: Path): Exception | Array =
    for {
      _metadata ← untyped.Metadata(dir)
    } yield
      new Array {
        type T = metadata.dtype.T

        val metadata = _metadata
        override val shape = metadata.shape
        val chunkShape = metadata.chunks

        private val _chunks = mutable.Map[String, ByteBuffer]()
        def chunk(idx: String): ByteBuffer =
          _chunks.getOrElseUpdate(
            idx,
            ByteBuffer.wrap(dir / idx readBytes)
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
          val chunkIdxBuilder = new StringBuilder
          while (idx < N) {
            if (idx == 0) chunkIdxBuilder += '.'
            chunkIdxBuilder ++= (idxs(idx) / chunkShape(idx)).toString
            sum += (idxs(idx) % chunkShape(idx)) * products(idx)
            idx += 1
          }

          val chunkIdx = chunkIdxBuilder.result

          metadata.dtype(chunk(chunkIdx), sum)
        }
      }
}
