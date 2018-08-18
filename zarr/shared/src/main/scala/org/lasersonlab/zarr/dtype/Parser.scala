package org.lasersonlab.zarr.dtype

import io.circe.Decoder.Result
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.hammerlab.lines.Name
import org.lasersonlab.zarr.dtype.ByteOrder.Endianness
import org.lasersonlab.zarr.dtype.Parser.Return
import org.lasersonlab.zarr.{ Int, | }

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

  import DataType._

  implicit val   _char: Parser[  Char] = make { case           '|' :: 'i' :: Int(   1) ⇒   char       }
  implicit val  _short: Parser[ Short] = make { case Endianness(e) :: 'i' :: Int(   2) ⇒  short(   e) }
  implicit val     int: Parser[   Int] = make { case Endianness(e) :: 'i' :: Int(   4) ⇒    i32(   e) }
  implicit val    long: Parser[  Long] = make { case Endianness(e) :: 'i' :: Int(   8) ⇒    i64(   e) }
  implicit val  _float: Parser[ Float] = make { case Endianness(e) :: 'f' :: Int(   4) ⇒  float(   e) }
  implicit val _double: Parser[Double] = make { case Endianness(e) :: 'f' :: Int(   8) ⇒ double(   e) }
  implicit val _string: Parser[String] = make { case           '|' :: 'S' :: Int(size) ⇒ string(size) }

  import shapeless._

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

  implicit def structParser[S, L <: HList](
    implicit
    g: Generic.Aux[S, L],
    l: StructParser[L]
  ):
        Parser[S] =
    new Parser[S] {
      def apply(c: HCursor): Return[S] =
        c
          .as[Vector[StructEntry]]
          .map {
            _.map { _.`type` }
          }
          .flatMap {
            arr ⇒
              l(arr.toList)
                .map(
                  l ⇒
                    DataType.struct[S, L](g, Lazy(l))
                )
          }
    }
}

