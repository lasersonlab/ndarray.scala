package org.lasersonlab

import cats.{ Eval, Foldable, Traverse }
import cats.implicits._
import com.tom_e_white.hdf5_java_cloud.NioReadOnlyRandomAccessFile
import hammerlab.bytes._
import hammerlab.either._
import hammerlab.option._
import hammerlab.path.Path
import io.circe.{ Encoder, Json }
import org.lasersonlab.netcdf.{ Attribute, Group, Variable }
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.Save
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.zarr.{ Attrs, Compressor, Dimension, FillValue, Key, Metadata }
import _root_.shapeless.the
import ucar.ma2.IndexIterator
import ucar.nc2.NetcdfFile

import scala.Array.fill
import scala.math.{ max, min }
import scala.util.Try

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

  implicit def convertGroup(
    group: Group
  )(
    implicit
    compressor: Compressor,
    chunkSize: Bytes
  ):
    zarr.Group[Int] =
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
    zarr.Array.List[Int] =
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
          if (_chunkSize.bytes > Int.MaxValue)
            throw new IllegalArgumentException(
              s"Chunks should be <2GB; got ${Bytes.format(_chunkSize)}"
            )
          else
            _chunkSize.bytes.toInt

        def dimensions(shape: Seq[Int], dtype: DataType) =
          shape
            .map {
              shape ⇒
                Dimension(
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
        val numRows = shape.head.arr

        val shapeTail =
          shape
            .tail
            .map(_.arr)

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
          implicit val traverseShape: Traverse[ShapeT] = cats.instances.list.catsStdInstancesForList

          val shape: List[Dimension[Int]] = tpe.shape

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

          // TODO: move this out
          implicit val foldArray: Foldable[Array] =
            new Foldable[Array] {
              type F[A] = Array[A]
              @inline override def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  = fa.foldLeft(b)(f)
              @inline override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.foldRight(lb)(f)
            }

          override def save(dir: Path): Throwable | Unit = {
            def chunkResults =
              chunks
                .zipWithIndex
                .map {
                  case (chunk, idx) ⇒
                    val basename = {
                      // since we chunk by rows only, chunk indices will always be all zeros except for their first
                      // coordinate
                      val idxs = fill(ndims)(0)
                      idxs(0) = idx
                      Key(idxs)
                    }

                    Try {
                      val path = dir / basename
                      path.mkdirs

                      import java.nio.ByteBuffer._
                      val buffer = allocate(datatype.size * chunk.shape.product)

                      chunk.foldLeft(()) {
                        (_, elem) ⇒

                          datatype(buffer, elem)
                        ()
                      }
                    }
                    .toEither
                }
                .sequence

            // TODO: optionally write to a tmp dir then "commit" to intended destination
            for {
              _ ← (metadata: Metadata[T, ShapeT, Idx]).save(dir) // TODO: rm type params
              _ ←   attrs.save(dir)
              _ ← chunkResults
            } yield
              ()
          }

          @inline override def apply(idx: List[Int]): T = {
            require(idx.size == rank, s"Index of size ${idx.size} (${idx.mkString(",")}) for array of rank $rank")
            val h :: t = idx
            val chunk = chunks(h / rowsPerChunk)
            chunk((h % rowsPerChunk) :: t)
          }

          override val attrs: Opt[Attrs] = _attrs
        }
    }

  /**
   * Wrapper for some properties, and a dependent type, that are being converted from HDF5
   */
  sealed abstract class Type {
    type T
    def shape: List[Dimension[Int]]
    def sectionShape: Array[Int]
    def datatype: DataType.Aux[T]
    def fill_value: FillValue[T]
    def next(it: IndexIterator): T
    implicit def encoder: FillValue.Encoder[T]
  }
  object Type {
    def make[_T](
      _shape: List[Dimension[Int]],
      _sectionShape: Array[Int],
      _datatype: DataType.Aux[_T],
      _fill_value: FillValue[_T],
      _next: IndexIterator ⇒ _T
    )(
      implicit
      _encoder: FillValue.Encoder[_T]
    ): Type =
      new Type {
        type T = _T
        val shape = _shape
        val sectionShape = _sectionShape
        val datatype = _datatype
        val fill_value = _fill_value
        val encoder = _encoder
        @inline def next(it: IndexIterator): T = _next(it)
      }
  }

  /**
   * Zarr "chunk" implementation that wraps a section of a NetCDF [[ucar.nc2.Variable]]
   *
   * @param variable underlying dataset
   * @param start chunk "origin"
   * @param shape chunk shape
   * @param sectionShape the shape of one element in the chunk; this is generally an array with `rank` copies of the
   *                     literal `1`, but in the case of converting char-arrays to fixed-length strings,
   * @param next callback for pulling the next element from an [[IndexIterator]]
   * @tparam T constituent element type
   */
  case class Chunk[T](
    variable: ucar.nc2.Variable,
    start: Int,
    shape: Array[Int],
    sectionShape: Array[Int]
  )(
    val next: IndexIterator ⇒ T
  ) {
    val rank = shape.length
    require(
      sectionShape.length <= rank + 1,
      s"Section shape (${sectionShape.mkString(",")}) is ${sectionShape.length - rank} longer than rank $rank (row: $start, shape ${shape.mkString(",")})"
    )
    def apply(idxs: Seq[Int]): T = {
      require(
        idxs.size == rank,
        s"Index of size ${idxs.size} (${idxs.mkString(",")}) for array of rank $rank"
      )

      val builder = Array.newBuilder[Int]
      for {
        i ← 0 until rank
         idx =  idxs(i)
        size = shape(i)
      } {
        val idx =
          if (i == 0)
            idxs.head + start
          else
            idxs(i)
        if (idx >= size)
          throw new IllegalArgumentException(
            s"Index $idx is larger than chunk-size $size (row $start; shape ${shape.mkString(",")}"
          )

        builder += idx
      }

      if (sectionShape.length == rank + 1)
        builder += 0

      val origin = builder.result()

      next(
        variable
          .read(origin, sectionShape)
          .getIndexIterator
      )
    }
  }
  object Chunk {
    implicit val foldable: Foldable[Chunk] =
      new Foldable[Chunk] {
        def foldLeft [A, B](fa: Chunk[A], b: B)(f: (B, A) ⇒ B): B = {
          import fa._
          val origin = Array.fill(sectionShape.length)(0)
          val shape =
            if (sectionShape.length == rank + 1)
              fa.shape :+ sectionShape.last
            else
              fa.shape
          origin(0) = start
          val data =
            variable
              .read(
                origin,
                shape
              )
              .getIndexIterator

          var ret = b
          while (data.hasNext) {
            ret = f(ret, next(data))
          }
          ret
        }
        def foldRight[A, B](fa: Chunk[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
      }
  }
}
