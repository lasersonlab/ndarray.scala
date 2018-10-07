package org.lasersonlab.zarr.utils

import io.circe.{ Decoder, Encoder }
import org.lasersonlab.ndarray.Arithmetic
import org.lasersonlab.ndarray.Arithmetic.Id
import org.lasersonlab.zarr.untyped.|
import org.lasersonlab.zarr.utils.Idx.Long.CastException

sealed trait Idx {
  type T
  implicit val arithmeticId: Arithmetic.Id[T]
  implicit val arithmeticInt: Arithmetic[T, Int]
  def int(t: T): CastException | Int
  implicit val encoder: Encoder[T]
  implicit val decoder: Decoder[T]
}
object Idx {
  type T[_T] = Idx { type T = _T }
  object Int
    extends Idx {
    type T = scala.Int
    val arithmeticId: Id[Int] = Arithmetic.intint
    val arithmeticInt: Arithmetic[Int, Int] = Arithmetic.intint
    def int(t: Int): CastException | Int = Right(t)
    val encoder = io.circe.Encoder.encodeInt
    val decoder = io.circe.Decoder.decodeInt
  }
  object Long
    extends Idx {
    case class CastException(value: Long)
      extends RuntimeException(
        s"Attempting to case $value to an integer"
      )
    type T = scala.Long
    val arithmeticId: Id[Long] = Arithmetic.longlong
    val arithmeticInt: Arithmetic[Long, Int] = Arithmetic.longint
    def int(t: Long): CastException | Int =
      if (t > Integer.MAX_VALUE)
        Left(
          CastException(t)
        )
      else
        Right(
          t.toInt
        )
    val encoder = io.circe.Encoder.encodeLong
    val decoder = io.circe.Decoder.decodeLong
  }

  object helpers {
    // TODO: remove explicit imports of this, mix it in instead
    implicit def specify(implicit idx: Idx): Idx.T[idx.T] = idx
  }

  // TODO: would be nice to not need all these, but afaict each set is necessary depending whether an `Idx` is
  // identified as `Idx.T` or not
  trait syntax {
    implicit def unwrapEncoder    (implicit idx: Idx     ): Encoder[idx.T] = idx.encoder
    implicit def unwrapDecoder    (implicit idx: Idx     ): Decoder[idx.T] = idx.decoder
    implicit def unwrapEncoderT[T](implicit idx: Idx.T[T]): Encoder[    T] = idx.encoder
    implicit def unwrapDecoderT[T](implicit idx: Idx.T[T]): Decoder[    T] = idx.decoder
    @inline def makeIdxOps[T](t: T): Ops[T] = Ops(t)
  }
  object syntax extends syntax

  implicit class Ops[T](val t: T) extends AnyVal {
    def int(implicit idx: Idx.T[T]): CastException | Int = idx.int(t)
  }
}
