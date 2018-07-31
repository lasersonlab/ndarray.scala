package org.lasersonlab.zarr

case class ChunkSize(sizes: Seq[Int]) {
  override val toString: String = sizes.mkString(".")
  val product =
    sizes
      .scanRight(1) { _ * _ }
      .drop(1)
}
object ChunkSize {
  implicit def unwrap(chunk: ChunkSize): Seq[Int] = chunk.sizes

  case class Key(override val toString: String)
  implicit def chunkToKey(chunk: ChunkSize): Key = Key(chunk.toString)
}
