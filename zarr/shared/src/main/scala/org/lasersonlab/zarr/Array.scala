package org.lasersonlab.zarr

import cats.data.Nested
import cats.{ Eval, Foldable, Traverse }
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.circe.{ CodecK, EncoderK }
import org.lasersonlab.ndarray.ArrayLike
import org.lasersonlab.shapeless.{ Scannable, Zip }
import org.lasersonlab.zarr
import org.lasersonlab.zarr.array.metadata
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.{ Load, Save }
import org.lasersonlab.zarr.untyped.FlatArray
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.zarr.utils.Idx.Long.CastException
import shapeless.Nat

import scala.util.Try

/**
 * A Zarr N-dimensional array
 *
 * Storage of the ND-array of chunks, as well as the records in each chunk, are each a configurable type-param; see
 * companion-object for some convenient constructors
 */
trait Array {
  /** Element type */
  type T

  /** Structure of this [[Array]]'s [[Shape "shape"]] (and, as a result, [[Index indices]] of individual elements) */
  type ShapeT[_]

  /**
   * "Index" data-type; typically [[Int]], but [[Long]] is also supported
   *
   * Note that the number of chunks in any dimension, as well as the size of each chunk in each dimension, must still be
   * an [[Int]]
   */
  type Idx

  /** Type of the "shape" of this [[Array]] */
  type Shape = ShapeT[Idx]

  /** Type of elements' indices; same as the [[Array]]'s [[Shape]] */
  type Index = Shape

  /** N-dimensional container for this [[Array]]'s "chunks" */
  type A[_]

  /** N-dimensional container that each "chunk" uses to store its [[T elements]] */
  type Chunk[_]

  /**
   * Useful evidences:
   *
   * - [[chunks the chunks array]] is [[Traverse traversable]]
   * - [[ShapeT the "shape" type]] is [[Traverse traversable]]
   * - a [[Chunk]] is [[Foldable]]
   */
  implicit val traverseA: Traverse[A]
  implicit val traverseShape: Traverse[ShapeT]
  implicit val foldableChunk: Foldable[Chunk]

  /**
   * Widen to an [[Array.T]], so that [[cats]] typeclasses (e.g. [[Array.foldableT]]) can be picked up, and
   * corresponding syntax used, e.g.
   *
   * {{{
   * arr.t.toList
   * }}}
   *
   * This is necessary due to some unification limitations relating to the various aliases ([[Array.Untyped]],
   * [[Array.Aux]], [[Array.List]], etc.) used to specify different subsets of an [[Array]]'s dependent types' that are
   * known at a given call-site
   */
  def t: Array.T[this.T] = this
  def aux: Array.Aux[ShapeT, Idx, A, Chunk, T] = this

  /**
   * Short-hand for imbuing this [[Array]] with an element type at runtime, e.g. in the case where it was loaded without
   * that type having been known ahead of time
   */
  def as[_T]: Array.Aux[this.ShapeT, this.Idx, this.A, this.Chunk, _T] =
    this
      .asInstanceOf[
        Array.Aux[
          this.ShapeT,
          this.Idx,
          this.A,
          this.Chunk,
          _T
        ]
      ]

  val shape: ShapeT[Dimension[Idx]]
  val chunkRanges: ShapeT[Chunk.Idx]

  /**
   * Random-indexing operation
   */
  def apply(idx: Index): T  // TODO: support exceptions: IndexOutOfBounds, IO, etc.

  val metadata: Metadata[ShapeT, Idx, T]

  /**
   * All the [[Array]]'s data lives here: an N-dimensional array of [[Chunk chunks]] that contain elements of type [[T]]
   */
  val chunks: A[Chunk[T]]

  // TODO: make this type-parameterizable, and validate accordingly during JSON-parsing
  val attrs: Opt[Attrs]

  def foldLeft[B](b: B)(f: (B, T) ⇒ B): B =
    chunks
      .foldLeft(b) {
        (b, chunk) ⇒
          chunk
            .foldLeft(b) { f }
      }

  def foldRight[B](lb: Eval[B])(f: (T, Eval[B]) ⇒ Eval[B]): Eval[B] =
    chunks
      .foldRight(lb) {
        _.foldRight(_) { f }
      }
}

object Array {

  /**
   * "Aux" aliases for various subsets of [[Array]]'s type-members that might be known / required in various contexts
   */
  type       T[                                    _T] = Array {                                                   type T = _T }
  type     Aux[_ShapeT[_], _Idx, _A[_], _Chunk[_], _T] = Array { type ShapeT[U] =    _ShapeT[U]; type Idx = _Idx ; type T = _T ; type A[U] = _A[U]; type Chunk[U] = _Chunk[U] }
  type      Of[_ShapeT[_], _Idx,                   _T] = Array { type ShapeT[U] =    _ShapeT[U]; type Idx = _Idx ; type T = _T }
  type Untyped[_ShapeT[_], _Idx                      ] = Array { type ShapeT[U] =    _ShapeT[U]; type Idx = _Idx }
  type    List[            _Idx                      ] = Array { type ShapeT[U] = scala.List[U]; type Idx = _Idx }

  /**
   * De-structure an [[Array]] into its [[Metadata]], [[Attrs]], and [[Array.chunks]] members, preserving whatever is
   * known about their types
   */
  def unapply(a: Array):
    Option[
      (
        Metadata[a.ShapeT, a.Idx, a.T],
        Option[Attrs],
        a.A[a.Chunk[a.T]]
      )
    ] =
    Some(
      (
        a.metadata,
        a.attrs,
        a.chunks
      )
    )

  /**
   * Load an ND-array of chunks from a [[Path directory]]
   *
   * Each chunk lives in a file with basename given by '.'-joined indices
   */
  private def chunks[
    Shape[_]: Traverse : Zip : Scannable,
      Idx,
       _T,
        A[_]: Traverse
  ](
      dir: Path,
    shape: Shape[Dimension[Idx]]
  )(
   implicit
   indices: Indices.Aux[A, Shape],
   _idx: Idx.T[Idx],
   compressor: Compressor,
   datatype: DataType.Aux[_T],
  ):
    Exception |
    A[
      Chunk[
        Shape,
        _T
      ]
    ]
  = {
    import _idx._
    val sizeHint = shape.foldLeft(1) { _ * _.chunk }
    for {
      arr ←
        indices(
          shape
            .map { _.range }
        )
        .map {
          idx: Shape[Chunk.Idx] ⇒
            import Zip.Ops
            for {
              chunkShape ←
                // chunks in the last "row" of any dimension may be smaller
                shape
                  .zip(idx)
                  .map {
                    case (Dimension(arr, chunk, _), idx) ⇒
                      val start = idx * chunk
                      val end = arr min ((idx + 1) * chunk)

                      int { end - start }
                  }
                  .sequence

              basename = Key(idx)

              chunk ←
                Chunk(
                  dir / basename,
                  chunkShape,
                  idx,
                  compressor,
                  sizeHint * datatype.size
                )
            } yield
              chunk
          }
          .sequence  // A[Err | Chunk] -> Err | A[Chunk]
    } yield
      arr
  }

  /**
   * Convenience-constructor: given a data-type and a [[Nat (type-level) number of dimensions]], load an [[Array]] from
   * a [[Path directory]]
   *
   * Uses a [[VectorEvidence]] as evidence for mapping from the [[Nat]] to a concrete shape
   *
   * Differs from [[apply]] above in that it returns full-resolved [[Array.A]] and [[Array.Chunk]] type-members, for
   * situations where that is important (in general, it shouldn't be; tests may wish to verify / operate on chunks, but
   * users shouldn't ever need to).
   */
  def tni[  // TODO: rename
    T,
    N <: Nat,
    Idx  // TODO: move this to implicit evidence
  ](
    dir: Path
  )(
    implicit
     v: VectorEvidence[N, Idx],
     d:  DataType.Decoder[T],
    dt: FillValue.Decoder[T],
  ):
    Exception |
    Aux[v.ShapeT, Idx, v.A, Chunk[v.ShapeT, ?], T]
  = {
    import v._
    apply[T, v.ShapeT, v.A](dir)(
      // shouldn't have to list all these explicitly: https://github.com/scala/bug/issues/11086
                  d = d,
                 ti = ti,
           traverse = traverse,
          arrayLike = arrayLike,
                 dt = dt,
         shapeCodec = shapeCodec,
                idx = idx,
      traverseShape = traverseShape,
           zipShape = zipShape,
          scannable = scannable
    )
  }

  def apply[
    _T,
    _Shape[_],
    _A[_]
  ](
    dir: Path
  )(
    implicit
                d: DataType.Decoder[_T],
               ti: Indices.Aux[_A, _Shape],
         traverse: Traverse[_A],
        arrayLike: ArrayLike.Aux[_A, _Shape],
               dt: FillValue.Decoder[_T],
       shapeCodec: CodecK[_Shape],
              idx: Idx,
    traverseShape: Traverse[_Shape],
         zipShape: Zip[_Shape],
        scannable: Scannable[_Shape]
  ):
    Exception |
    Aux[
      _Shape,
      idx.T,
      _A,
      Chunk[
        _Shape,
        ?
      ],
      _T
    ]
  =
    for {
      _metadata ← {
        import Idx.helpers.specify
        dir.load[Metadata[_Shape, idx.T, _T]]
      }
      arr ← {
        import Idx.helpers.specify
        Array(dir, _metadata)
      }
    } yield
      arr

  /**
   * Load an [[Array]] whose element-type and number of dimensions are unknown
   *
   * Dimensions are loaded as a [[List]], and the element-type is loaded as a type-member `T`
   */
  def untyped(
    dir: Path
  )(
    implicit
    idx: Idx
  ):
    Exception |
    Array.List[idx.T]
  =
    metadata.untyped(dir)
      .flatMap {
        metadata ⇒
          import Idx.helpers.specify
          apply[
            metadata.T,
            scala.List,
            idx.T,
            FlatArray
          ](
            dir,
            metadata.t
          )
          .map {
            arr ⇒ arr: List[idx.T]
          }
      }

  def apply[
        _T
    ,
    _Shape[_]
          : Scannable
          : Zip
          : Traverse
          : EncoderK
    ,
      _Idx,
        _A[_]
  ](
    dir: Path,
    _metadata: Metadata[_Shape, _Idx, _T]
  )(
    implicit
    ti: Indices.Aux[_A, _Shape],
    _traverse: Traverse[_A],
    arrayLike: ArrayLike.Aux[_A, _Shape],
    idx: Idx.T[_Idx]
  ):
    Exception |
    Aux[
      _Shape,
      _Idx,
      _A,
      Chunk[
        _Shape,
        ?
      ],
      _T
    ]
  =
    for {
      _attrs ← dir.load[Opt[Attrs]]
      _chunks ← {
        implicit val md = _metadata
        import Metadata._
        chunks[_Shape, _Idx, _T, _A](
          dir,
          _metadata.shape
        )
      }
    } yield
      new Array {
        type T = _T
        type Idx = _Idx
        type ShapeT[U] = _Shape[U]
        type A[U] = _A[U]
        type Chunk[U] = zarr.Chunk[ShapeT, U]

        val traverseA     = Traverse[     A]
        val traverseShape = Traverse[ShapeT]

        val foldableChunk = Chunk.foldable

        import idx._

        val metadata = _metadata
        val datatype =  metadata.dtype
        val   chunks =   _chunks
        val    attrs =    _attrs

        val shape = metadata.shape

        val chunkRanges =
          shape
            .map { _.range }

        import lasersonlab.shapeless.slist._

        def apply(idx: Shape): T = {
          import Zip.Ops

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
                    int { idx / chunk } ::
                    int { idx % chunk } ::
                    ⊥
                }                    : F[G[E[T]]]
            )
            .sequence               // E[F[G[T]]]
            .map(_.value.sequence)  // E[G[F[T]]]
            .right.get               :   G[F[T]]

          arrayLike(
            chunks,
            chunkIdx
          )(
            offset
          )
        }
      }

  /**
   * Implement [[Foldable]] on an [[Array]] identified only by its element type; part of working around / mitigating
   * https://github.com/scala/bug/issues/11169.
   *
   * [[Array.foldLeft foldLeft]] and [[Array.foldRight foldRight]] are defined directly on [[Array]], but this can still
   * be useful in contexts based around Cats typeclasses
   */
  implicit val foldableT: Foldable[Array.T] =
    new Foldable[Array.T] {
      @inline def foldLeft [A, B](fa: T[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  = fa.foldLeft ( b)(f)
      @inline def foldRight[A, B](fa: T[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.foldRight(lb)(f)
    }

  // TODO: would be nice to not need multiple overloads corresponding to different "Aux" aliases
  implicit def loadArrInt[
    Shape[_],
        T
          :  DataType.Decoder
          : FillValue.Decoder
        ,
        N <: Nat
  ](
    implicit
     v: VectorEvidence.Ax[N, Shape, Int]
  ):
    Load[
      lasersonlab.zarr.Array[Shape, T]
    ] =
    loadArr[Shape, Int, T, N]

  implicit def loadArr[
    Shape[_],
    Idx,
    T
      :  DataType.Decoder
      : FillValue.Decoder
    ,
    N <: Nat
  ](
    implicit
     v: VectorEvidence.Ax[N, Shape, Idx]
  ):
    Load[
      Of[Shape, Idx, T]
    ] =
    new Load[Of[Shape, Idx, T]] {
      override def apply(dir: Path): Exception | Of[Shape, Idx, T] =
        Array.tni[T, N, Idx](dir)
    }

  implicit def saveOf[
    Shape[_]
            : EncoderK
            : Scannable,
  ](
    implicit
    idx: Idx
  ):
    Save[
      Untyped[Shape, idx.T]
    ] =
    new Save[
      Untyped[
        Shape,
        idx.T
      ]
    ] {
      def apply(
        a: Untyped[Shape, idx.T],
        dir: Path
      ):
        Throwable |
        Unit
      =
        save[
          Shape,
          a.A,
          a.Chunk,
          a.T
        ]
        .apply(
          a,
          dir
        )
    }

  implicit def save[
    _Shape[_]
            : EncoderK
            : Scannable,
         A[_],
     Chunk[_],
        _T
  ](
    implicit
    idx: Idx
  ):
    Save[
      Aux[_Shape, idx.T, A, Chunk, _T]
    ] =
    new Save[Aux[_Shape, idx.T, A, Chunk, _T]] {
      type _Idx = idx.T
      def apply(
        a: Aux[_Shape, idx.T, A, Chunk, _T],
        dir: Path
      ):
        Throwable |
        Unit
      = {
        // TODO: these implicit vals shouldn't be necessary; minimize+file
        implicit val ta: Traverse[     A] = a.traverseA
        implicit val ts: Traverse[_Shape] = a.traverseShape
        implicit val fc: Foldable[ Chunk] = a.foldableChunk

        import a._

        def chunkResults: Throwable | Unit = {
          import Scannable.Ops
          val (_, chunkStrides) = chunkRanges.scanRight(1)(_ * _)
          val chunkSize =
            shape
              .foldLeft(1) {
                _ * _.chunk
              }

          a
            .aux
            .chunks
            .mapWithIndex {
              (chunk, int) ⇒
                // traverse chunk-strides to convert linear/integer index to structured N-dimensional index
                val idx =
                  chunkStrides
                    .scanLeft_→(
                      (
                        int,  // output
                        int
                      )
                    ) {
                      case (
                        (
                          _,
                          remaining
                        ),
                        stride
                      ) ⇒
                        (
                          remaining / stride,
                          remaining % stride
                        )
                    }
                    .map { _._1 }

                val path = dir / Key(idx)
                Try {
                  import java.nio.ByteBuffer._
                  val datatype = a.metadata.dtype
                  val buffer = allocate(datatype.size * chunkSize)
                  chunk
                    .foldLeft(()) {
                      (_, elem) ⇒
                        datatype(buffer, elem)

                        ()
                    }

                  val os =
                    a.metadata.compressor(
                      path.outputStream(mkdirs = true),
                      datatype.size
                    )

                  os.write(buffer.array())
                  os.close()
                }
                .toEither
            }
            .sequence[Throwable | ?, Unit]
            .map { _ ⇒ () }
        }

        // TODO: configure ability to write to a temporary location and then "commit" all results
        for {
          _ ← a.metadata.save(dir)
          _ ← a.attrs.save(dir)
          _ ← chunkResults
        } yield
          ()
      }
    }
}
