package org.lasersonlab.zarr.dtype

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.hammerlab.lines.Name
import org.lasersonlab.zarr.dtype.ByteOrder.Endianness
import org.lasersonlab.zarr.dtype.Parser.Return
import org.lasersonlab.zarr.untyped.Struct
import org.lasersonlab.zarr.{ Int, | }

/**
 * Parse some JSON to identify and construct a [[DataType]] for a [[T given type]]
 */
trait Parser[T] {
  def apply(c: HCursor): Return[T]
}
object Parser {
  type Return[T] = DecodingFailure | DataType.Aux[T]

  def make[T](
    fn:
      PartialFunction[
        List[Char],
        DataType.Aux[T]
      ]
  )(
    implicit
    name: Name[T]
  ):
    Parser[T] =
    new Parser[T] {
      @inline def apply(c: HCursor): Return[T] =
        c
          .value
          .as[String]
          .flatMap {
            str ⇒
              fn
                .andThen(Right(_))
                .applyOrElse[List[Char], Return[T]](
                  str.toList,
                  str ⇒
                    Left(
                      DecodingFailure(
                        s"Unrecognized $name dtype: ${str.mkString}",
                        c.history
                      )
                    )
                )
          }
    }

  import DataType.{ Struct ⇒ _, _ }

  implicit val   _byte: Parser[  Byte] = make { case           '|' :: 'i' :: Int(   1) ⇒   byte       }
  implicit val  _short: Parser[ Short] = make { case Endianness(e) :: 'i' :: Int(   2) ⇒  short(   e) }
  implicit val    _int: Parser[   Int] = make { case Endianness(e) :: 'i' :: Int(   4) ⇒ int(e) }
  implicit val   _long: Parser[  Long] = make { case Endianness(e) :: 'i' :: Int(   8) ⇒   long(e) }
  implicit val  _float: Parser[ Float] = make { case Endianness(e) :: 'f' :: Int(   4) ⇒  float(   e) }
  implicit val _double: Parser[Double] = make { case Endianness(e) :: 'f' :: Int(   8) ⇒ double(   e) }
  implicit val _string: Parser[String] = make { case           '|' :: 'S' :: Int(size) ⇒ string(size) }

  import shapeless._

  /**
   * The JSON representation of a struct field
   */
  case class StructEntry(name: String, `type`: String)
  object StructEntry {
    implicit val decoder: Decoder[StructEntry] =
      new Decoder[StructEntry] {
        def apply(c: HCursor): Result[StructEntry] =
          c
            .as[Vector[String]]
            .flatMap {
              case Vector(name, tpe) ⇒
                Right(
                  StructEntry(
                    name,
                    tpe
                  )
                )
              case v ⇒
                Left(
                  DecodingFailure(
                    s"Array has unexpected number of elements (${v.length}): ${v.mkString(",")}",
                    Nil
                  )
                )
            }
      }
  }

  implicit def structParser[S, L <: HList, D <: HList](
    implicit
    g: Generic.Aux[S, L],
    l: StructParser[L]
  ):
        Parser[S] =
    new Parser[S] {
      def apply(c: HCursor): Return[S] =
        c
          .as[Vector[StructEntry]]
          .flatMap {
            entries ⇒
              l(entries.toList)
                .map {
                  struct(g, _)
                }
          }
    }

  implicit val untypedStruct: Parser[Struct] =
    new Parser[Struct] {
      import cats.implicits._
      override def apply(c: HCursor): Return[Struct] =
        c
          .value
          .as[
            Vector[
              StructEntry
            ]
          ]
          .flatMap {
            _
              .map {
                case StructEntry(name, tpe) ⇒
                  get(
                    tpe,
                    c
                  )
                  .map {
                    DataType.StructEntry(name, _)
                  }
              }
              .sequence[
                Result,
                DataType.StructEntry
              ]
              .map { untyped.Struct(_) }
          }
    }
}
