package org.lasersonlab.zarr.dtype

import cats.implicits._
import io.circe
import io.circe.Decoder.Result
import io.circe.{ DecodingFailure, HCursor, Json }
import org.hammerlab.lines.Name
import org.lasersonlab.zarr.dtype.ByteOrder.Endianness
import org.lasersonlab.zarr.dtype.DataType._
import org.lasersonlab.zarr.dtype.json.Entry
import org.lasersonlab.zarr.|

object json {
  /**
   * The JSON representation of a struct field
   */
  case class Entry(name: String, `type`: String) {
    override def toString: String = s"[$name,${`type`}]"
  }
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
    g: LabelledGeneric.Aux[S, L],
    l: StructParser[L]
  ):
  Decoder[S] =
    new circe.Decoder[DataType[S]] {
      def apply(c: HCursor): Return[S] =
        c
          .as[Vector[Entry]]
          .flatMap {
            entries ⇒
              l(entries.toList)
                .bimap(
                  DecodingFailure(_, c.history),
                  Struct(g, _)
                )
          }
    }
}

trait Coders
  extends StructDecoder {

  type Decoder[T] = circe.Decoder[DataType[T]]
  type Encoder[T] = circe.Encoder[DataType[T]]

  /**
   * Decode a [[DataType]] when its constituent type is not known ahead of time
   */
  implicit val decoder: circe.Decoder[DataType.?] =
    new circe.Decoder[DataType.?] {
      def apply(c: HCursor): Result[DataType.?] =
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
  implicit def dataTypeEncoder[D <: DataType.?]: circe.Encoder[D] =
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
          case      p: Primitive[_]           ⇒ Json.fromString(p.toString)
          case struct(StructList(entries, _)) ⇒ seq(entries)
          case struct.?         (entries    ) ⇒ seq(entries)
        }
    }

  type Return[T] = DecodingFailure | DataType[T]

  def make[T](
    fn:
      PartialFunction[
        List[Char],
        DataType[T]
      ]
  )(
    implicit
    name: Name[T]
  ):
    Decoder[T] =
    new circe.Decoder[DataType[T]] {
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

  object Int {
    def unapply(s: Seq[Char]): Option[Int] = Some( s.mkString.toInt )
  }

  implicit val _decodeByte   : Decoder[  Byte] = make { case           '|' :: 'i' :: Int(   1) ⇒   byte       }
  implicit val _decodeShort  : Decoder[ Short] = make { case Endianness(e) :: 'i' :: Int(   2) ⇒  short(   e) }
  implicit val _decodeInt    : Decoder[   Int] = make { case Endianness(e) :: 'i' :: Int(   4) ⇒    int(   e) }
  implicit val _decodeLong   : Decoder[  Long] = make { case Endianness(e) :: 'i' :: Int(   8) ⇒   long(   e) }
  implicit val _decodeFloat  : Decoder[ Float] = make { case Endianness(e) :: 'f' :: Int(   4) ⇒  float(   e) }
  implicit val _decodeDouble : Decoder[Double] = make { case Endianness(e) :: 'f' :: Int(   8) ⇒ double(   e) }
  implicit val _decodeString : Decoder[String] = make { case           '|' :: 'S' :: Int(size) ⇒ string(size) }

  import org.lasersonlab.zarr.{ untyped ⇒ u }
  implicit val untypedStructDecoder: Decoder[u.Struct] =
    new circe.Decoder[DataType[u.Struct]] {
      override def apply(c: HCursor): Result[struct.?] =
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
              .sequence
              .map {struct.? }
          }
    }
}
