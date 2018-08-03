package org.lasersonlab.ndarray

import java.io.{ ByteArrayOutputStream, DataOutputStream, OutputStream }
import java.nio.ByteBuffer

trait Read[T] {
  /**
   * Read the record of type [[T]] at index `idx` in the array `bytes`
   *
   * `idx` is *not* the offset in the byte-array to start reading at; it refers to the `idx`-th record in the array of
   * serialized records; this leaves room for implementations that e.g. store records in variable numbers of bytes
   */
  def apply(buff: ByteBuffer, idx: Int): T
}
object Read {
  implicit val int: Read[Int] =
    new Read[Int] {
      val size = 4
      @inline def apply(buff: ByteBuffer, idx: Int): Int = buff.getInt(idx * size)
    }
}

trait Put[T] {
  def apply(buff: ByteBuffer, idx: Int, t: T): Unit
  def apply(os: DataOutputStream, t: T): Unit
}
object Put {
  implicit val int: Put[Int] =
    new Put[Int] {
      val size = 4
      def apply(buff: ByteBuffer, idx: Int, t: Int): Unit = buff.putInt(idx * size, t)
      override def apply(os: DataOutputStream, t: Int): Unit = os.writeInt(t)
    }
}

trait Write[T] {
  def apply(t: T): scala.Array[Byte]
}
object Write {
  implicit def array[Arr, Elem, Idx](
    implicit
    arr: ToArray.Aux[Arr, Elem, Idx],
    traverse: Traverse[Arr, Elem],
    put: Put[Elem]
  ):
    Write[Arr] =
    new Write[Arr] {
      def apply(t: Arr): scala.Array[Byte] = {
        val baos = new ByteArrayOutputStream()
        val data = new DataOutputStream(baos)
        traverse(t).foreach {
          put(data, _)
        }
        data.close()
        baos.toByteArray
      }
    }

  implicit class Ops[T](val t: T) extends AnyVal {
    def write(implicit ev: Write[T]): scala.Array[Byte] = ev(t)
  }
}
