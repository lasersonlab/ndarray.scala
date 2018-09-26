package org.lasersonlab.zarr.dtype

import cats.implicits._
import io.circe
import io.circe.Decoder.Result
import io.circe.{ DecodingFailure, HCursor, Json }
import org.hammerlab.lines.Name
import org.lasersonlab.zarr.dtype.ByteOrder.Endianness
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.dtype.json.Entry
import org.lasersonlab.zarr.{ Int, | }

object json {
  /**
   * The JSON representation of a struct field
   */
  case class Entry(name: String, `type`: String)
  object Entry {
    implicit val decoder: circe.Decoder[Entry] =
      new circe.Decoder[Entry] {
        def apply(c: HCursor): Result[Entry] =
          c
            .as[Vector[String]]
            .flatMap {
              case Vector(name, tpe) ⇒
                Right(
                  Entry(
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
}

trait StructDecoder {
  import shapeless._
  implicit def structDecoder[S, L <: HList, D <: HList](
    implicit
    g: Generic.Aux[S, L],
    l: StructParser[L]
  ):
  Decoder[S] =
    new circe.Decoder[Aux[S]] {
      def apply(c: HCursor): Return[S] =
        c
        .as[Vector[Entry]]
          .flatMap {
            entries ⇒
              // TODO: verify field-names (optionally?)
              l(entries.toList)
                .map {
                  struct(g, _)
                }
          }
    }
}

trait Coders
  extends StructDecoder {

  type Decoder[T] = circe.Decoder[Aux[T]]
  type Encoder[T] = circe.Encoder[Aux[T]]

  /**
   * Decode a [[DataType]] when its constituent type is not known ahead of time
   */
  implicit val decoder: circe.Decoder[DataType] =
    new circe.Decoder[DataType] {
      import cats.implicits._
      def apply(c: HCursor): Result[DataType] =
        c
        .value
        .as[String]
          .fold(
            _ ⇒ untypedStructDecoder(c),
            get(_, c)
          )
    }

  /**
   * Encode any datatype as JSON
   */
  implicit def dataTypeEncoder[D <: DataType]: circe.Encoder[D] =
    new circe.Encoder[D] {
      def seq(entries: Seq[StructEntry]): Json =
        Json.arr(
          entries
            .map {
              case StructEntry(name, datatype) ⇒
                Json.arr(
                  Json.fromString(name),
                  dataTypeEncoder(datatype)
                )
            }
            : _*
        )
      def apply(a: D): Json =
        a match {
          case p: Primitive[_] ⇒ Json.fromString(p.toString)
          case Struct(StructList(entries, _)) ⇒ seq(entries)
          case untyped.Struct   (entries    ) ⇒ seq(entries)
        }
    }

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
    Decoder[T] =
    new circe.Decoder[Aux[T]] {
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

  implicit val _decodeByte   : Decoder[  Byte] = make { case           '|' :: 'i' :: Int(   1) ⇒   byte       }
  implicit val _decodeShort  : Decoder[ Short] = make { case Endianness(e) :: 'i' :: Int(   2) ⇒  short(   e) }
  implicit val _decodeInt    : Decoder[   Int] = make { case Endianness(e) :: 'i' :: Int(   4) ⇒    int(   e) }
  implicit val _decodeLong   : Decoder[  Long] = make { case Endianness(e) :: 'i' :: Int(   8) ⇒   long(   e) }
  implicit val _decodeFloat  : Decoder[ Float] = make { case Endianness(e) :: 'f' :: Int(   4) ⇒  float(   e) }
  implicit val _decodeDouble : Decoder[Double] = make { case Endianness(e) :: 'f' :: Int(   8) ⇒ double(   e) }
  implicit val _decodeString : Decoder[String] = make { case           '|' :: 'S' :: Int(size) ⇒ string(size) }

  import org.lasersonlab.zarr.{ untyped ⇒ u }
  implicit val untypedStructDecoder: Decoder[u.Struct] =
    new circe.Decoder[Aux[u.Struct]] {
      override def apply(c: HCursor): Result[untyped.Struct] =
        c
          .value
          .as[
            List[
              Entry
            ]
          ]
          .flatMap {
            _
              .map {
                case Entry(name, tpe) ⇒
                  get(
                    tpe,
                    c
                  )
                  .map {
                    StructEntry(name, _)
                  }
              }
              .sequence[
                Result,
                StructEntry
              ]
              .map { untyped.Struct }
          }
    }
}
