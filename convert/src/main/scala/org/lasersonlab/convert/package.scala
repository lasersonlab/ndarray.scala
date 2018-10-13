package org.lasersonlab

import _root_.cats.implicits._
import _root_.cats.{ Foldable, Traverse }
import _root_.shapeless.the
import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import hammerlab.bytes._
import hammerlab.option._
import hammerlab.path.Path
import io.circe.Json
import lasersonlab.zarr.Group
import org.lasersonlab.netcdf.{ Attribute, Variable }
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.Save
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.zarr.{ Attrs, Compressor, Dimension, FillValue, Metadata }
import ucar.ma2.IndexIterator
import ucar.nc2.NetcdfFile

import scala.Array.fill
import scala.math.{ max, min }

package object convert
  extends Save.syntax
{
  implicit def loadHDF5(path: Path): netcdf.Group =
    NetcdfFile.open(
      new NioReadOnlyRandomAccessFile(path),
      path.toString,
      null,
      null
    )
    .getRootGroup

  implicit val __int = Idx.Int

  implicit def convertGroup(
    group: netcdf.Group
  )(
    implicit
    compressor: Compressor,
    chunkSize: Bytes
  ):
    Group =
    new zarr.Group[Int](
      arrays =
        group
          .vars
          .map {
            v ⇒
              v.name →
                convertVariable(v)
          }
          .toMap,
      groups =
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
    Option[Attrs] =
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
    zarr.Array.??[Int] =
    variable match {
      case Variable(
        name,
        description,
        dtype,
        _attrs,
        dimensions,
        rank,
        _shape,
        size,
        data
      ) ⇒
        import DataType._
        import ucar.ma2.DataType._

        val chunkSize =
          if (_chunkSize.bytes > scala.Int.MaxValue)
            throw new IllegalArgumentException(
              s"Chunks should be <2GB; got ${Bytes.format(_chunkSize)}"
            )
          else
            _chunkSize.bytes.toInt

        def dimensions(shape: Seq[Int], dtype: DataType) =
          shape
            .map {
              shape ⇒
                Dimension.int(
                  shape,
                  max(
                    1,
                    min(
                      shape,
                      chunkSize / dtype.size
                    )
                  )
                )
            }
            .toList

        val tpe: Type =
          dtype match {
            // fixed-length strings come in as CHARs with an extra dimensions
            case   CHAR ⇒
              val size = _shape.last

              new Type {
                type T = String

                val datatype = string(size)
                val fill_value = FillValue("")
                val encoder = the[FillValue.Encoder[T]]

                val shape = dimensions(_shape.dropRight(1), datatype)

                val sectionShape = fill(rank)(1)
                sectionShape(rank - 1) = size

                def next(it: IndexIterator): String = {
                  var i = 0
                  val sb = new StringBuilder
                  while (it.hasNext && i < size) {
                    sb += it.getCharNext
                    i += 1
                  }
                  if (i != size)
                    throw new IllegalStateException(
                      s"Only read $i characters (expected $size)"
                    )
                  sb.result()
                }
              }
            case    INT ⇒ Type.make ( dimensions(_shape,    int), fill(rank)(1),    int, 0        , { it: IndexIterator ⇒ it.getIntNext    } )
            case   LONG ⇒ Type.make ( dimensions(_shape,   long), fill(rank)(1),   long, 0L       , { it: IndexIterator ⇒ it.getLongNext   } )
            case  SHORT ⇒ Type.make ( dimensions(_shape,  short), fill(rank)(1),  short, 0: Short , { it: IndexIterator ⇒ it.getShortNext  } )
            case   BYTE ⇒ Type.make ( dimensions(_shape,   byte), fill(rank)(1),   byte, 0: Byte  , { it: IndexIterator ⇒ it.getByteNext   } )
            case  FLOAT ⇒ Type.make ( dimensions(_shape,  float), fill(rank)(1),  float, 0.0f     , { it: IndexIterator ⇒ it.getFloatNext  } )
            case DOUBLE ⇒ Type.make ( dimensions(_shape, double), fill(rank)(1), double, 0.0      , { it: IndexIterator ⇒ it.getDoubleNext } )
            case c ⇒
              throw new UnsupportedOperationException(
                s"Unimplemented datatype: $dtype"
              )
          }

        import tpe._

        val ndims = shape.size

        val elemsPerChunk = max(1, chunkSize / datatype.size)
        val numRows = shape.head.size

        val shapeTail =
          shape
            .tail
            .map(_.size)

        val rowSize = shapeTail.product

        val rowsPerChunk =
          min(
            numRows,
            max(
              1,
              elemsPerChunk / rowSize
            )
          )

        // TODO: add /↑ helper for rounding-up division
        val numChunks = (numRows + rowsPerChunk - 1) / rowsPerChunk

        val _metadata =
          Metadata(
            shape = shape,
            dtype = datatype,
            compressor = compressor,
            order = C,
            fill_value = fill_value
          )

        new zarr.Array {
          override type T = tpe.T
          type ShapeT[U] = List[U]

          type Idx = Int

          override val metadata = _metadata

          type     A[U] =        Vector[U]  // we're only chunking by "row" / major-axis; 1-D array of chunks is fine
          type Chunk[U] = convert.Chunk[U]

          implicit val traverseA: Traverse[A] = catsStdInstancesForVector
          implicit val foldableChunk: Foldable[Chunk] = Chunk.foldable
          implicit val traverseShape: Traverse[ShapeT] = catsStdInstancesForList

          val shape: List[Dimension[Int]] = tpe.shape

          val chunkRanges = shape.map(_.range)

          val chunks: A[Chunk[T]] =
            (0 until numChunks)
              .map {
                i ⇒
                  val start = i * rowsPerChunk
                  val end = min(numRows, (i+1) * rowsPerChunk)
                  val rows = end - start

                  val shape = rows :: shapeTail toArray

                  Chunk[T](
                    data,
                    start,
                    shape,
                    sectionShape
                  )(
                    next
                  )
              }
              .toVector

          @inline override def apply(idx: List[Int]): T = {
            require(idx.size == rank, s"Index of size ${idx.size} (${idx.mkString(",")}) for array of rank $rank")
            val h :: t = idx
            val chunk = chunks(h / rowsPerChunk)
            chunk((h % rowsPerChunk) :: t)
          }

          override val attrs: Option[Attrs] = _attrs
        }
    }
}
