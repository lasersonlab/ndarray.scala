package org.lasersonlab.zarr.dtype

import io.circe.{ DecodingFailure, HCursor, Json }
import org.lasersonlab.zarr.dtype.DataType.Decoder
import org.lasersonlab.zarr.|
import shapeless._
import DataType.StructList
import org.lasersonlab.zarr.dtype.json.Entry
import shapeless.labelled.FieldType

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
     Name <: Symbol,
     Head,
     Tail <: HList,
    DTail <: HList
  ](
    implicit
    head: Decoder[Head],
    w: Witness.Aux[Name],
    tail: StructParser[Tail]
  ):
        StructParser[FieldType[Name, Head] :: Tail] =
    new StructParser[FieldType[Name, Head] :: Tail] {
      def apply(entries: List[Entry]): Return[FieldType[Name, Head] :: Tail] =
        entries match {
          case Nil ⇒
            Left(
              DecodingFailure(
                "Ran out of elements",
                Nil
              )
            )
          case scala.::(Entry(name, tpe), rest) ⇒
            if (name == w.value.name)
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
                DataType.cons[Name, Head, Tail](h, w, t)
            else {
              Left(
                DecodingFailure(
                  s"Expected field name ${w.value.name}, found $name (type: $tpe); entries: ${entries.mkString(",")}",  // TODO
                  Nil   // TODO
                )
              )
            }
        }
    }
}
