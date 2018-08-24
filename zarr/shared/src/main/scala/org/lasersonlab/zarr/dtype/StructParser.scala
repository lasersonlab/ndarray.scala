package org.lasersonlab.zarr.dtype

import io.circe.{ DecodingFailure, HCursor, Json }
import org.lasersonlab.zarr.dtype.Parser.StructEntry
import org.lasersonlab.zarr.|
import shapeless._
import DataType.StructList

trait StructParser[L <: HList, D <: HList] {
  import StructParser.Return
  def apply(c: List[StructEntry]): Return[L, D]
}

object StructParser {
  import DataType.Struct
  type Return[L <: HList, D <: HList] = DecodingFailure | StructList[L, D]

  implicit val hnil:
        StructParser[HNil, HNil] =
    new StructParser[HNil, HNil] {
      def apply(c: List[StructEntry]): Return[HNil, HNil] =
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
    // TODO: making these both Lazy breaks [[Parser.structParser]] derivation; why?
    head: Parser[Head],
    tail: StructParser[Tail, DTail]
  ):
        StructParser[Head :: Tail, DataType.StructEntry[Head] :: DTail] =
    new StructParser[Head :: Tail, DataType.StructEntry[Head] :: DTail] {
      def apply(entries: List[StructEntry]): Return[Head :: Tail, DataType.StructEntry[Head] :: DTail] =
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
                head(
                  HCursor.fromJson(
                    Json.fromString(
                      tpe
                    )
                  )
                )
              t ← tail(rest)
            } yield
              DataType.cons[Head, Tail, DTail](h, t)
        }
    }
}
