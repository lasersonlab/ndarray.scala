package org.lasersonlab.zarr.dtype

import io.circe.{ HCursor, Json }
import org.lasersonlab.zarr.dtype.DataType.{ Decoder, StructList }
import org.lasersonlab.zarr.dtype.json.Entry
import org.lasersonlab.zarr.|
import shapeless._
import shapeless.labelled.FieldType

/**
 * Parse an [[L HList]] from a list of JSON [[Entry field-entries]]
 */
trait StructParser[L <: HList] {
  import StructParser.Return
  def apply(c: List[Entry]): Return[L]
}

object StructParser {
  type Return[L <: HList] = String | StructList[L]

  implicit val hnil:
        StructParser[HNil] =
    new StructParser[HNil] {
      def apply(c: List[Entry]): Return[HNil] =
        c match {
          case Nil ⇒ Right(DataType.hnil)
          case   l ⇒  Left(s"${l.size} extra elements: ${l.mkString(",")}")
        }
    }

  def fieldNamesMatch(code: String, data: String): Boolean =
    data.toLowerCase.filter(_ != '_') == code.toLowerCase

  implicit def cons[
     Name <: Symbol,
     Head,
     Tail <: HList
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
          case Nil ⇒ Left("Ran out of elements")
          case scala.::(Entry(name, tpe), rest) ⇒
            if (fieldNamesMatch(w.value.name, name))
              for {
                h ←
                  head(
                    HCursor.fromJson(
                      Json.fromString(
                        tpe
                      )
                    )
                  )
                  .left
                  .map(_.message)
                t ← tail(rest)
              } yield
                DataType.cons[Name, Head, Tail](h, w, t)
            else
              Left(
                s"Expected field name ${w.value.name}, found $name (type: $tpe); entries: ${entries.mkString(",")}"
              )
        }
    }
}
