package org.lasersonlab.zarr.array

import org.lasersonlab.zarr._
import cats.Traverse
import cats.data.Nested
import lasersonlab.zarr.Path
import org.lasersonlab.circe.EncoderK
import org.lasersonlab.ndarray.{ ArrayLike, Indices, Vector }
import org.lasersonlab.slist.{ Scannable, Zip }
import org.lasersonlab.zarr.Array
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.{ zarr ⇒ z }

import scala.concurrent.ExecutionContext

trait load {
  self: Array.type ⇒
  /**
   * Load an ND-array of chunks from a [[Path directory]]
   *
   * Each chunk lives in a file with basename given by '.'-joined indices
   */
  private def chunks[
    ShapeT[_]
            : Traverse
            : Zip
            : Scannable,
       Idx  : Idx.T,
         T,
         A[_]
            : Traverse
  ](
      dir: Path,
    shape: ShapeT[Dimension[Idx]]
  )(
   implicit
      indices:    Indices[A, ShapeT],
     datatype:   DataType[      T],
   compressor: Compressor,
           ec: ExecutionContext
  ):
    F[
      A[
        Chunk[
          ShapeT,
          T
        ]
      ]
    ]
  = {
    val sizeHint = shape.foldLeft(1) { _ * _.chunk }
    indices(
      shape
        .map {
          _.range
        }
    )
    .map {
      idx ⇒
        val chunkShape =
          // chunks in the last "row" of any dimension may be smaller
          shape
            .zip(idx)
            .map {
              case (Dimension(arr, chunk, _), idx) ⇒
                val start = idx * chunk
                val end = arr min ((idx + 1) * chunk)

                { end - start } int
            }
            .sequence
            .fold(throw _, x ⇒ x)

        val basename = Key(idx)

        Chunk(
          dir / basename,
          chunkShape,
          idx,
          compressor,
          sizeHint * datatype.size
        )
    }
    .sequence  // A[F[Chunk]] -> F[A[Chunk]]
  }

  /**
   * Main constructor: given [[ShapeT shape]]- and [[T data]]-types, load an [[Array]] from a [[Path directory]]
   *
   * @param dir path to load as a Zarr [[Array]]
   * @param v provides an "N-D array"-type to store chunks in, as well as relevant evidence for using [[ShapeT]]s with
   *          it
   * @param idx "index" type to use (e.g. [[Int]] or [[Long]])
   * @tparam ShapeT "shape" of the [[Array]]; also of elements' indices
   * @tparam T element-type of this [[Array]]
   */
  def apply[
    ShapeT[_],
         T
         :  DataType.Decoder
         : FillValue.Decoder
  ](
    dir: Path
  )(
    implicit
     _v: VectorEvidence[ShapeT],
    idx:            Idx,
    ec: ExecutionContext
  ):
    F[
      Aux[
        ShapeT,
        idx.T,
        _v.A,
        Chunk[ShapeT, ?],
        T
      ]
    ]
  = {
    // HKT-members in method parameters are considered "unstable", so we have to work around it this way (or e.g. pass
    // all implicit params explicitly / by name); see https://github.com/scala/bug/issues/11086
    val v = _v
    import v._
    implicit val ev = v.t
    apply[ShapeT, A, T](dir)
      .map {
        _.asInstanceOf[
          Aux[
            ShapeT,
            idx.T,
            _v.A,  // _v.A isn't recognized as being equal to v.A, due to https://github.com/scala/bug/issues/11086
            Chunk[ShapeT, ?],
            T
          ]
        ]
      }
  }

  def apply[
    ShapeT[_],
         A[_],
         T
         :  DataType.Decoder
         : FillValue.Decoder
  ](
    dir: Path
  )(
    implicit
    idx: Idx,
     ev: VectorEvidence.make[ShapeT, A],
     ec: ExecutionContext
  ):
    F[
      Aux[
        ShapeT,
        idx.T,
        A,
        Chunk[ShapeT, ?],
        T
      ]
    ]
  = {
    import ev.{ A ⇒ _, _ }
    for {
      metadata ←
        dir.load[
          Metadata[
            ShapeT,
            idx.T,
            T
          ]
        ]
      arr ← Array(dir, metadata)
    } yield
      arr
  }

  /**
   * Load an [[Array]] whose element-type and number of dimensions are unknown
   *
   * Dimensions are loaded as a [[List]], and the element-type is loaded as a type-member ("T")
   */
  def ?(
    dir: Path
  )(
    implicit
    idx: Idx,
    ec: ExecutionContext
  ):
    F[Array.*?[idx.T]]
  =
    metadata
      .?(dir)
      .flatMap {
        metadata ⇒
          apply[
            List,
            idx.T,
            Vector.*,
            metadata.T
          ](
            dir,
            metadata.t
          )
          .map {
            arr ⇒ arr: Array.*?[idx.T]
          }
      }

  def apply[
    ShapeT[_]
            : Scannable
            : Zip
            : EncoderK,
      Idx  : Idx.T,
        A[_],
        T
  ](
    dir: Path,
    _metadata: Metadata[ShapeT, Idx, T]
  )(
    implicit
          indices:   Indices    [A, ShapeT],
        arrayLike: ArrayLike.Aux[A, ShapeT],
         traverse:  Traverse    [A        ],
    traverseShape:  Traverse    [   ShapeT],
               ec: ExecutionContext
  ):
    F[
      Aux[
        ShapeT,
        Idx,
        A,
        Chunk[
          ShapeT,
          ?
        ],
        T
      ]
    ]
  =
    for {
      _attrs ← dir.load[Option[Attrs]]
      _chunks ← {
        implicit val md = _metadata
        import Metadata._
        chunks[ShapeT, Idx, T, A](
          dir,
          _metadata.shape
        )
      }
    } yield
      new Aux[ShapeT, Idx, A, z.Chunk[ShapeT, ?], T] {
        val metadata = _metadata
        val   chunks =   _chunks
        val    attrs =    _attrs

        val shape = metadata.shape

        def apply(idx: Shape): T = {

          // aliases for annotating the `.sequence` shenanigans below
          type E[U] = CastException | U
          type F[U] = ShapeT[U]
          type G[U] = `2`[U]
          type T = Chunk.Idx

          // traverse the dimensions in the requested idx, as well as this array's shape, to obtain the chunk index and
          // the intra-chunk offset (each of which has a component along each dimension)
          val chunkIdx :: offset ::  ⊥ =
            Nested(
              idx
                .zip(shape)
                .map {
                  case (idx, Dimension(_, chunk, _)) ⇒
                    { idx / chunk }.int ::
                    { idx % chunk }.int ::
                    ⊥
                }                    : F[G[E[T]]]
            )
            .sequence               // E[F[G[T]]]
            .map(_.value.sequence)  // E[G[F[T]]]
            .right.get               :   G[F[T]]

          chunks(
            chunkIdx
          ).apply(
            offset
          )
        }
      }
}
