package org.lasersonlab.zarr.array

import cats.{ Foldable, Traverse }
import hammerlab.option._
import org.lasersonlab.ndarray.{ Indices, UnfoldRange, Vector }
import org.lasersonlab.slist.{ Scannable, Size, Zip }
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.FillValue.Null
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr._
import org.lasersonlab.zarr.circe.Json
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.utils.ChunkSize
import shapeless.the

trait make {
  self: Array.type ⇒
  /**
   * Constructor for "in-line" / in-memory creation of [[Array]]s
   *
   * The [[ShapeT shape]], [[T elements]], and an implicit [[DataType]] are the only required parameters
   *
   * Many other parameters can be specified, either explicitly / by-name in the first parameter list, or via implicit
   * instances (the former overrides the latter)
   *
   * The second parameter (in the first parameter list), `chunkSize`, may also be used positionally; if not specified,
   * it defaults to the `shape` (first parameter), resulting in an [[Array]] that is one chunk
   */
  def apply[
    ShapeT[_]
    : Scannable
    : Size
    : UnfoldRange
    : Zip,
    T
  ](
         shape:         ShapeT[Int]        ,
     chunkSize: Opt[    ShapeT[Int]] = None,
        _attrs: Opt[      Json     ] = None,
         dtype: Opt[  DataType[  T]] = None,
    compressor: Opt[Compressor     ] = None,
         order: Opt[     Order     ] = None,
    fill_value: Opt[ FillValue[  T]] = None
  )(
    _elems: Seq[T]
  )(
    implicit
       _datatype:          DataType[T],
     _compressor:        Compressor    = Blosc(),
          _order:             Order    = C,
     _fill_value:         FillValue[T] = Null,
     zarr_format:            Format    = Format.`2`,
         filters: Option[Seq[Filter]]  = None,
      _chunkSize:         ChunkSize    = 32 MB,
    traverseShape: Traverse[ShapeT]
  ):
    Aux[
      ShapeT,
      Int,
      Vector[ShapeT, ?],
      Vector[ShapeT, ?],
      T
    ] = {
    val _shape = shape  // work around shadowing, but keep parameter-name nice
    new Aux[
      ShapeT,
      Int,
      Vector[ShapeT, ?],
      Vector[ShapeT, ?],
      T
    ] {
      val datatype = dtype.getOrElse(_datatype)

      val chunkShape =
        chunkSize
          .getOrElse {
            // If an explicit chunkSize isn't passed, chunk along the first axis, taking as many "rows" as still allow
            // chunks to be ≤ the implicit `ChunkSize` value (which defaults to 32MB)
            val bytes = _chunkSize.size
            val rowElems = _shape.toList.tail.product
            val rowSize = rowElems * datatype.size
            val rowsPerChunk =
              math.max(
                1,
                bytes / rowSize
              )

            _shape
              .mapWithIndex {
                (s, i) ⇒
                  if (i == 0)
                    math.min(s, rowsPerChunk)
                  else
                    s
              }
          }

      override val shape: ShapeT[Dimension[Idx]] =
        _shape
          .zip(chunkShape)
          .map {
            case (shape, chunk) ⇒
              Dimension.int(shape, chunk)
          }

      val elems = Vector[ShapeT, T](_shape, _elems: _*)

      override def apply(idx: Index): T = elems(idx)

      override val metadata: Metadata[ShapeT, Idx, T] =
        Metadata(
                shape = shape,
                dtype =   datatype,
           compressor = compressor.getOrElse(_compressor),
                order =      order.getOrElse(     _order),
           fill_value = fill_value.getOrElse(_fill_value),
          zarr_format = zarr_format,
              filters = filters
        )

      val indices = the[Indices[A, ShapeT]]
      override val chunks: A[Chunk[T]] = {
        indices(chunkRanges)
          .map {
            chunkIdx ⇒
              val start :: size :: end :: ⊥ =
                chunkShape
                  .zip(chunkIdx, _shape)
                  .map {
                    case (chunkSize, idx, size) ⇒
                      val start = chunkSize * idx
                      val end = (chunkSize * (idx + 1)) min size
                      start :: (end - start) :: end :: ⊥
                  }
                  .sequence

              indices(size)
                .map {
                  offset ⇒
                    apply(
                      start
                        .zip(offset)
                        .map {
                          case (start,  offset ) ⇒
                                start + offset }
                    )
                }
          }
      }

      override val attrs: Option[Attrs] = _attrs.map(Attrs(_))
    }
  }
}
