package org.lasersonlab.zarr.untyped

import java.io.DataOutputStream
import java.nio.ByteBuffer

import cats.implicits._
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.group.Save
import org.lasersonlab.zarr.{ Attrs, | }

import scala.collection.mutable
import scala.util.Try

trait Array {
  type T
  def rank = metadata.rank
  def metadata: Metadata.Aux[T]
  def datatype: DataType.Aux[T]
  def attrs: Opt[Attrs]
  def apply(idxs: Int*): T

  import hammerlab.math.utils.div

  def chunkIndexRanges: List[Range] =
    metadata
      .shape
      .zip(metadata.chunks)
      .map {
        case (shape, chunk) ⇒
          0 until div(shape, chunk)
      }
      .toList

  def chunkIndices = indices(chunkIndexRanges)

  def indices(ranges: List[Range]): Iterator[List[Int]] =
    ranges match {
      case Nil ⇒ Iterator()
      case h :: t ⇒
        for {
          h ← h.iterator
          t ← indices(t)
        } yield
          h :: t
    }

  def chunkRange(chunkIdx: Seq[Int]): List[Range] = {
    require(chunkIdx.size == rank)
    metadata
      .shape
      .zip(metadata.chunks)
      .zip(chunkIdx)
      .map {
        case ((shape, chunkShape), chunkIdx) ⇒
          (chunkIdx * chunkShape) until
          math.min(
            shape,
            (chunkIdx + 1) * chunkShape
          )
      }
      .toList
  }

  def chunkElems(chunkIdx: Seq[Int]): Iterator[T] =
    indices(
      chunkRange(chunkIdx)
    )
    .map(
      apply(_: _*)
    )
}

object Array {
  type Aux[_T] = Array { type T = _T }

  implicit def unwrap[T](a: Aux[T]): Metadata.Aux[T] = a.metadata

  import org.lasersonlab.zarr.group.Load.Ops

  def apply(dir: Path): Exception | Array =
    for {
      _metadata ← Metadata(dir)
      _attrs ← dir.load[Opt[Attrs]]
    } yield
      new Array {

        val metadata = _metadata
        val datatype = _metadata.dtype
        val attrs = _attrs

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
      }: Array

  implicit val save: Save[Array] =
    new Save[Array] {
      import Save.Ops
      def apply(t: Array, dir: Path): Throwable | Unit = {
        def chunks: Throwable | List[Unit] =
          (
            for {
              chunkIdx ← t.chunkIndices
            } yield
              Try {
                val path = dir / chunkIdx.mkString(".")
                val os = new DataOutputStream(path.outputStream(mkdirs = true))
                t
                  .chunkElems(chunkIdx)
                  .foreach {
                    t.datatype.apply(os, _)
                  }

                ()
              }
              .toEither
          )
          .toList
          .sequence

        import io.circe.generic.auto._

        for {
          _ ← (t.metadata: Metadata).save(dir)
          _ ← t.attrs.save(dir)
          _ ← chunks
        } yield
          ()
      }
    }
}
