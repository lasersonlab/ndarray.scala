package org.lasersonlab.zarr

import org.lasersonlab.ndarray
import hammerlab.option._
import org.lasersonlab.ndarray.{ Bytes, ToArray, ToList, Zip }


case class Chunk[T, Shape](
  override val bytes: scala.Array[Byte],
  override val shape: Shape
)(
  implicit
  dtype: DataType[T],
  toList: ToList[Int, Shape]
)
extends Bytes[T, Shape](bytes, shape)

//case class Chunk[T, Shape](
//  data: scala.Array[Byte],
//  shape: Shape
//)(
//  implicit
//  dtype: DataType[T]
//) {
//  def apply(idx: List[Int], pos: Int = 0): T =
//    idx match {
//      case Nil ⇒ dtype(data, pos)
//      case h :: t ⇒ ???
//    }
//}
//object Chunk {
//  implicit def toArray[T, Shape]: ToArray.Aux[Chunk[T, Shape], T, Shape] =
//    ToArray(
//      _.shape,
//      (chunk, idx) ⇒
//    )
//}

case class Array[T, Shape](
  metadata: Metadata[T, Shape],
  chunks: ndarray.Array[Chunk[T, Shape]],
  attrs: Opt[Attrs] = None
)(
  implicit val toList: ToList[Int, Shape]
) {
//  def apply(idx: List[Int], pos: Int = 0): Chunk[T] =
//    idx match {
//      case Nil ⇒ ???
//      case h :: t ⇒ ???
//    }
}

object Array {
//  type Chunk[T, Shape] = Bytes[T, Shape]

  implicit def toArray[T, Shape](implicit zip: Zip[Shape, Shape]): ToArray.Aux[Array[T, Shape], T, Shape] =
    ToArray[Array[T, Shape], T, Shape](
      _.metadata.shape,
      (arr, idx) ⇒ {
        val metadata = arr.metadata
        val zipped = zip(metadata.shape, metadata.chunks)

        val (chunkIdx, elemIdx) =
          arr
            .toList(
              arr.metadata.shape
            )
            //.iterator
            .zip(
              arr
                .toList(
                  arr.metadata.chunks
                )
                //.iterator
            )
            .map {
              case (arr, chunk) ⇒
                (
                  arr / chunk,
                  arr % chunk
                )
            }
            .unzip
        arr.chunks(idx)
      }
    )
}
