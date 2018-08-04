package org.lasersonlab.zarr

import hammerlab.option._
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray
import org.lasersonlab.ndarray.{ Bytes, ToArray }

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
  chunks: ndarray.Array.Aux[Chunk[T, Shape], Shape],
  attrs: Opt[Attrs] = None
)(
  implicit
  val toList: ToList[Int, Shape]
) {
//  def apply(idx: List[Int], pos: Int = 0): Chunk[T] =
//    idx match {
//      case Nil ⇒ ???
//      case h :: t ⇒ ???
//    }
}

object Array {

  implicit def toArray[
    T,
    Shape <: TList.Aux[Int],
    Zipped <: TList.Aux[(Int, Int)]
  ](
    implicit
    zip: Zip.Aux[Shape, Shape, Zipped],
    map: Map.Aux[Zipped, (Int, Int), Int, Shape]
  ):
    ToArray.Aux[
      Array[T, Shape],
      T,
      Shape
    ] =
    ToArray[Array[T, Shape], T, Shape](
      _.metadata.shape,
      (arr, idx) ⇒ {
        val metadata = arr.metadata
        val zipped = {
          metadata.shape.zip(metadata.chunks)
        }

        val chunkIdx = zipped.map { case (arr, chunk) ⇒ arr / chunk }
        val elemIdx = zipped.map { case (arr, chunk) ⇒ arr % chunk }

        arr.chunks(chunkIdx)(elemIdx)
      }
    )
}
