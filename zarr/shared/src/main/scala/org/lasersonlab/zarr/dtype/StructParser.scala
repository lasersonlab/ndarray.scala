package org.lasersonlab.zarr.dtype

import io.circe.{ DecodingFailure, HCursor, Json }
import org.lasersonlab.zarr.dtype.DataType.Decoder
import org.lasersonlab.zarr.|
import shapeless._
import DataType.StructList
import org.lasersonlab.zarr.dtype.json.Entry

/**
 * Parse an [[L HList]] from a list of JSON [[Entry field-entries]]
 */
trait StructParser[L <: HList] {
  import StructParser.Return
  def apply(c: List[Entry]): Return[L]
}

object StructParser {
  type Return[L <: HList] = DecodingFailure | StructList[L]

  implicit val hnil:
        StructParser[HNil] =
    new StructParser[HNil] {
      def apply(c: List[Entry]): Return[HNil] =
        c match {
          case Nil ⇒ Right(DataType.hnil)
          case l ⇒
            Left(
              DecodingFailure(
                s"${l.size} extra elements: ${l.mkString(",")}",
                Nil
              )
            )
        }
    }

  implicit def cons[
     Head,
     Tail <: HList,
    DTail <: HList
  ](
    implicit
    head: Decoder[Head],
    tail: StructParser[Tail]
  ):
        StructParser[Head :: Tail] =
    new StructParser[Head :: Tail] {
      def apply(entries: List[Entry]): Return[Head :: Tail] =
        entries match {
          case Nil ⇒
            Left(
              DecodingFailure(
                "Ran out of elements",
                Nil
              )
            )
          case scala.::(Entry(name, tpe), rest) ⇒
            for {
              h ←
                head(
                  HCursor.fromJson(
                    Json.fromString(
                      tpe
                    )
                  )
                )
              t ← tail(rest)
            } yield
              DataType.cons[Head, Tail](h, t)
        }
    }
}
