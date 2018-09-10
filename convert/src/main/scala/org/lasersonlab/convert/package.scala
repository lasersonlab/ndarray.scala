package org.lasersonlab

import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import hammerlab.bytes._
import hammerlab.option._
import hammerlab.path.Path
import io.circe.Json
import org.lasersonlab.netcdf.{ Attribute, Group, Variable }
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.untyped.Metadata
import org.lasersonlab.zarr.{ Attrs, Compressor, FillValue }
import ucar.nc2.NetcdfFile

import math.{ max, min }

package object convert {
  implicit def loadHDF5(path: Path): netcdf.Group =
    NetcdfFile.open(
      new NioReadOnlyRandomAccessFile(path),
      path.toString,
      null,
      null
    )
    .getRootGroup

  implicit def convertGroup(
    group: Group
  )(
    implicit
    compressor: Compressor,
    chunkSize: Bytes
  ):
    zarr.untyped.Group =
    zarr.untyped.Group(
      group
        .vars
        .map {
          v ⇒
            v.name →
              convertVariable(v)
        }
        .toMap,
      group
        .groups
        .map {
          g ⇒
            g.name →
              convertGroup(g)
        }
        .toMap,
      group.attributes
    )

  implicit def convertAttributes(
    attributes: Seq[Attribute]
  ):
    Opt[Attrs] =
    if (attributes.isEmpty)
      None
    else
      Some(
        Attrs {
          import Attribute._
          import io.circe.Encoder.encodeSeq
          Json.obj(
            attributes
              .map {
                case   Chars(name, values) ⇒ name → encodeSeq[  Char].apply(values)
                case   Bytes(name, values) ⇒ name → encodeSeq[  Byte].apply(values)
                case  Shorts(name, values) ⇒ name → encodeSeq[ Short].apply(values)
                case    Ints(name, values) ⇒ name → encodeSeq[   Int].apply(values)
                case   Longs(name, values) ⇒ name → encodeSeq[  Long].apply(values)
                case  Floats(name, values) ⇒ name → encodeSeq[ Float].apply(values)
                case Doubles(name, values) ⇒ name → encodeSeq[Double].apply(values)
                case Strings(name, values) ⇒ name → encodeSeq[String].apply(values)
              }
              : _*
          )
        }
      )


  implicit def convertVariable(
    variable: Variable
  )(
    implicit
    compressor: Compressor,
    _chunkSize: Bytes
  ):
    zarr.untyped.Array =
    variable match {
      case Variable(
        name,
        description,
        dtype,
        _attrs,
        dimensions,
        rank,
        shape,
        size,
        data
      ) ⇒
        import DataType._
        import ucar.ma2.DataType._

        val chunkSize =
          if (_chunkSize.bytes > Int.MaxValue)
            throw new IllegalArgumentException(
              s"Chunks should be <2GB; got ${Bytes.format(_chunkSize)}"
            )
          else
            _chunkSize.bytes.toInt

        val (_shape, _datatype, fill_value): (Seq[Int], DataType, FillValue[Json]) =
          dtype match {
            // fixed-length strings come in as CHARs with an extra dimensions
            case   CHAR ⇒ (shape.dropRight(1), string(shape.last), Json.fromString(""))
            case    INT ⇒ (shape             ,    int            , Json.fromInt(0))
            case   LONG ⇒ (shape             ,   long            , Json.fromInt(0))
            case  SHORT ⇒ (shape             ,  short            , Json.fromInt(0))
            case   BYTE ⇒ (shape             ,   byte            , Json.fromInt(0))
            case  FLOAT ⇒ (shape             ,  float            , Json.fromDoubleOrNull(0))
            case DOUBLE ⇒ (shape             , double            , Json.fromDoubleOrNull(0))
            case c ⇒
              throw new UnsupportedOperationException(
                s"Unimplemented datatype: $dtype"
              )
          }

        import Array.fill
        import ucar.ma2.Range

        val get: Seq[Int] ⇒ _datatype.T =
          _datatype match {
            case s @ string(size) ⇒
              // fixed-length strings come in as CHARs with an extra dimensions
              idxs ⇒
                val sectionSize = fill(rank)(0)
                sectionSize(rank - 1) = size

                val ranges = new java.util.ArrayList[Range](rank)

                idxs
                  .foreach {
                    idx ⇒
                      ranges
                        .add(
                          new Range(idx, idx)
                        )
                  }

                ranges.add(
                  new Range(
                    0,
                    shape.last - 1
                  )
                )

                val sb = new StringBuilder
                val chars = data.getRangeIterator(ranges)
                while (chars.hasNext) {
                  sb += chars.getCharNext
                }
                sb.result().asInstanceOf[_datatype.T]
            case _ ⇒
              idxs ⇒
                val index = data.getIndex
                index.set(idxs.toArray)
                index.currentElement()
                data.getObject(index).asInstanceOf[_datatype.T]
          }

        val elemsPerChunk = chunkSize / _datatype.size
        val shapeTail = _shape.tail.toList
        val rowSize = shapeTail.foldLeft(1L)(_ * _)
        val rowsPerChunk =
          min(
            _shape.head,
            max(
              1,
              (elemsPerChunk / rowSize).toInt
            )
          )

        val chunkShape = rowsPerChunk :: shapeTail

        val _metadata =
          Metadata(
            shape = _shape,
            chunks = chunkShape,
            dtype = _datatype,
            compressor = compressor,
            order = C,
            fill_value = fill_value
          )

        new zarr.untyped.Array {
          override type T = _datatype.T
          override val datatype: DataType.Aux[T] = _datatype
          override val metadata = _metadata
          @inline override def apply(idxs: Int*): T = get(idxs)
          override val attrs: Opt[Attrs] = _attrs
        }
    }
}
