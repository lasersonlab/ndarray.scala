package org.lasersonlab.zarr.dtype

import io.circe.{ DecodingFailure, HCursor, Json }
import org.lasersonlab.zarr.dtype.Parser.StructEntry
import org.lasersonlab.zarr.|
import shapeless._

trait StructParser[L <: HList] {
  import StructParser.Return
  def apply(c: List[StructEntry]): Return[L]
}

object StructParser {
  import DataType.Struct
  type Return[T] = DecodingFailure | Struct[T]

  implicit val hnil:
        StructParser[HNil] =
    new StructParser[HNil] {
      def apply(c: List[StructEntry]): Return[HNil] =
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
    Tail <: HList
  ](
    implicit
    head: Lazy[Parser[Head]],
    tail: Lazy[StructParser[Tail]]
  ):
        StructParser[Head :: Tail] =
    new StructParser[Head :: Tail] {
      def apply(entries: List[StructEntry]): Return[Head :: Tail] =
        entries match {
          case Nil ⇒
            Left(
              DecodingFailure(
                "Ran out of elements",
                Nil
              )
            )
          case scala.::(StructEntry(name, tpe), rest) ⇒
            for {
              h ←
                head.value(
                  HCursor.fromJson(
                    Json.fromString(
                      tpe)
                  )
                )
              t ← tail.value(rest)
            } yield
              DataType.cons[Head, Tail](h, Lazy(t))
        }
    }
}
