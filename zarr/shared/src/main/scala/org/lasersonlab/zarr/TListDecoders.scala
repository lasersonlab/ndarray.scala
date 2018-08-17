package org.lasersonlab.zarr

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }
import hammerlab.shapeless.tlist._

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