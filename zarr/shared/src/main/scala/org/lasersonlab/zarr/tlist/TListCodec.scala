package org.lasersonlab.zarr.tlist

import hammerlab.shapeless.tlist._
import io.circe.Decoder.Result
import io.circe._

// TODO: move to shapeless-utils?
trait TListDecoders {
  implicit val tnil: Decoder[TNil] =
    new Decoder[TNil] {
      override def apply(c: HCursor): Result[TNil] =
        c
          .value
          .as[List[Int]]
          .flatMap {
            case Nil ⇒ Right(TNil)
            case l ⇒
              Left(
                DecodingFailure(
                  s"Found extra elements: ${l.mkString(",")}",
                  c.history
                )
              )
          }
    }

  implicit def tlistDecoder[
    T,
    TL <: TList
  ](
    implicit
    dt: Decoder[T],
    fs: FromSeq.Aux[T, T :: TL]
  ):
    Decoder[T :: TL] =
    new Decoder[T :: TL] {
      def apply(c: HCursor): Result[T :: TL] =
        c
          .value
          .as[List[T]]
            .flatMap {
              fs(_)
                .left
                .map {
                  e ⇒
                    DecodingFailure(
                      e,
                      c.history
                    )
                }
            }
      }
}

object TListDecoders extends TListDecoders

trait TListEncoders {
  implicit def encodeAsSeq[T <: TList, E](
    implicit
    tolist: ToList.Aux[E, T],
    encode: Encoder[E]
  ):
    Encoder[T] =
    new Encoder[T] {
      def apply(t: T): Json =
        Json.arr(
          tolist(t)
            .map(
              encode(_)
            ):
            _*
        )
    }
}

object TListEncoders extends TListEncoders

trait TListCodec
  extends TListDecoders
     with TListEncoders

object TListCodec extends TListCodec
