package org.lasersonlab.zarr.untyped

import java.nio.ByteBuffer._

import cats.{ Applicative, Eval, Traverse }
import cats.implicits._
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.ndarray.ArrayLike
import org.lasersonlab.zarr
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.Save
import org.lasersonlab.zarr.{ Attrs, Indices }

import scala.util.Try

case class FlatArray[T](shape: Seq[Int], elems: Seq[T]) {
  val size :: strides = shape.scanRight(1)(_ * _).toList
  val rank = shape.size
  def apply(idx: Seq[Int]): T = {
    var sum = 0
    val idxs = idx.iterator
    var stride = strides
    while (stride != Nil) {
      sum += idxs.next * stride.head
      stride = stride.tail
    }
    if (idxs.hasNext)
      throw new IllegalArgumentException(
        s"Got index of rank ${idx.size} (${idx.mkString(",")}) in array of rank $rank"
      )
    elems(sum)
  }
}

object FlatArray {
  implicit val arrayLike: ArrayLike.Aux[FlatArray, Seq[Int]] =
    new ArrayLike[FlatArray] {
      type Shape = Seq[Int]
      @inline def shape(a: FlatArray[_]): Shape = a.shape
      @inline def apply[T](a: FlatArray[T], idx: Shape): T = a(idx)
    }

  implicit val seq: Indices.Aux[FlatArray, Seq[Int]] =
    new Indices[FlatArray] {
      type Shape = Seq[Int]
      @inline def apply(shape: Shape): FlatArray[Shape] =
        FlatArray(
          shape,
          rec(
            shape.toList
          )
        )
      private def rec(shape: List[Int]): List[List[Int]] =
        shape match {
          case Nil ⇒ Nil
          case scala.::(h, t) ⇒
            for {
              h ← (0 until h).toList
              t ← rec(t)
            } yield
              scala.::(h, t)
        }
    }

  implicit val traverse: Traverse[FlatArray] =
    new Traverse[FlatArray] {
      type F[A] = FlatArray[A]
      override def traverse[G[_], A, B](fa: F[A])(f: A ⇒ G[B])(implicit ev: Applicative[G]): G[F[B]] = {
        foldLeft(
          fa,
          ev.pure {
            Vector.newBuilder[B]
          }
        ) {
          (builder, elem) ⇒
            ev.map2(builder, f(elem)) { _ += _ }
        }
        .map {
          builder ⇒
            FlatArray(
              fa.shape,
              builder.result
            )
        }
      }
      @inline def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  = fa.elems.foldLeft ( b)(f)
      @inline def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.elems.foldRight(lb)(f)
    }
}

//trait Array {
//  type T
//  def metadata: Metadata.S[Seq[Int]]  // TODO: make this Aux[T]; so far unable to get the types to line up in HDF5-conversion code
//  def datatype: DataType.Aux[T]
//  def attrs: Opt[Attrs]
//  def apply(idxs: Int*): T
//
//  import hammerlab.math.utils.div
//
//  def chunkIndexRanges: List[Range] =
//    metadata
//      .shape
//      .zip(metadata.chunks)
//      .map {
//        case (shape, chunk) ⇒
//          0 until div(shape, chunk)
//      }
//      .toList
//
//  def chunkIndices = indices(chunkIndexRanges)
//
//  def indices(ranges: List[Range]): Iterator[List[Int]] =
//    ranges match {
//      case Nil ⇒ Iterator(Nil)
//      case h :: t ⇒
//        for {
//          h ← h.iterator
//          t ← indices(t)
//        } yield
//          h :: t
//    }
//
//  def chunkRange(chunkIdx: Seq[Int]): List[Range] = {
//    require(chunkIdx.size == metadata.chunks.size)
//    metadata
//      .shape
//      .zip(metadata.chunks)
//      .zip(chunkIdx)
//      .map {
//        case ((shape, chunkShape), chunkIdx) ⇒
//          (chunkIdx * chunkShape) until
//          math.min(
//            shape,
//            (chunkIdx + 1) * chunkShape
//          )
//      }
//      .toList
//  }
//
//  def chunkElems(chunkIdx: Seq[Int]): Iterator[T] =
//    indices(
//      chunkRange(chunkIdx)
//    )
//    .map(
//      apply(_: _*)
//    )
//
//  def elems: Iterator[T] =
//    for {
//      chunkIdx ← chunkIndices
//      elem ← chunkElems(chunkIdx)
//    } yield
//      elem
//}

object Array {
//  type Aux[_T] = Array { type T = _T }

  //implicit def unwrap[T](a: Aux[T]): Metadata.Aux[T] = a.metadata
//  implicit def unwrap[T](a: Aux[T]): Metadata = a.metadata

  import cats.implicits.catsKernelStdOrderForInt

  def apply(dir: Path): Exception | zarr.Array =
    Metadata(dir)
      .flatMap {
        metadata ⇒
          zarr.Array.md[
            metadata.T,
            Seq[Int],
            FlatArray
          ](
            dir,
            metadata,
            metadata.dtype
          )
          .map {
            arr ⇒ arr: zarr.Array
          }
      }

//      new zarr.Array {
//        type Shape = Seq[Int]
//
//        val metadata = _metadata
//        val datatype = _metadata.dtype
//        val attrs = _attrs
//
//        final type T = _metadata.dtype.T
//
//        val      shape = metadata.shape
//        val chunkShape = metadata.chunks
//
//        // TODO: LRU this / cap maximum size
//        private val _chunks = mutable.Map[Seq[Int], ByteBuffer]()
//        def chunk(idx: Seq[Int]): ByteBuffer =
//          _chunks.getOrElseUpdate(
//            idx,
//            ByteBuffer.wrap(
//              metadata.compressor(
//                dir / idx.mkString("."),
//                sizeHint = total * datatype.size
//              )
//            )
//          )
//
//        require(
//          shape.size == chunkShape.size,
//          s"Shape dimensions (${shape.size}: ${shape.mkString(",")}) don't match chunk dimensions (${chunkShape.size}: ${chunkShape.mkString(",")})"
//        )
//
//        val N = chunkShape.size
//
//        val total :: products =
//          chunkShape
//            .scanRight(1)(_ * _)
//            .toList
//
//        def apply(idxs: Seq[Int]): T = {
//          if (idxs.size != N)
//            throw new IllegalArgumentException(
//              s"Expected $N dimensions, found ${idxs.size}: ${idxs.mkString(",")}"
//            )
//
//          var idx = 0
//          var sum = 0
//          val chunkIdx = ArrayBuffer[Int]()
//
//          val shape = chunkShape.iterator
//          val is = idxs.iterator
//          val ps = products.iterator
//          while (idx < N) {
//            val i = is.next
//            val chunkDimension = shape.next
//            val stride = ps.next
//            chunkIdx += i / chunkDimension
//            sum += (i % chunkDimension) * stride
//            idx += 1
//          }
//
//          try {
//            _metadata.dtype.read(chunk(chunkIdx), sum)
//          } catch {
//            case e: Exception ⇒
//              throw new RuntimeException(
//                s"Failed to read index ${idxs.mkString(",")} (chunk ${chunkIdx.mkString(",")}, path ${dir / chunkIdx.mkString(".")}, offset $sum (${products.mkString(",")}))",
//                e
//              )
//          }
//        }
//      }: zarr.Array

//  implicit val save: Save[Array] =
//    new Save[Array] {
//      def apply(t: Array, dir: Path): Throwable | Unit = {
//        def chunks: Throwable | List[Unit] =
//          (
//            for {
//              chunkIdx ← t.chunkIndices
//            } yield
//              Try {
//                val path = dir / chunkIdx.mkString(".")
//                path.mkdirs
//
//                val datatype = t.datatype
//                val elems = t.chunkElems(chunkIdx)
//
//                val buffer = allocate(datatype.size * t.metadata.chunks.product)
//
//                elems
//                  .foreach {
//                    datatype(buffer, _)
//                  }
//
//                val os =
//                  t.compressor(
//                    path.outputStream,
//                    t.dtype.size
//                  )
//
//                os.write(buffer.array())
//
//                os.close()
//              }
//              .toEither
//          )
//          .toList
//          .sequence
//
//        import io.circe.generic.auto._
//
//        // TODO: optionally write to a tmp dir then "commit" to intended destination
//        for {
//          _ ← t.metadata.save(dir)
//          _ ← t.  attrs.save(dir)
//          _ ← chunks
//        } yield
//          ()
//      }
//    }
}
