package org.lasersonlab.zarr

import cats.data.Nested
import cats.{ Eval, Foldable, Traverse }
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.circe.{ CodecK, EncoderK }
import org.lasersonlab.ndarray.ArrayLike
import org.lasersonlab.shapeless.{ Scannable, Zip }
import org.lasersonlab.zarr.array.metadata
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.{ Load, Save }
import org.lasersonlab.zarr.untyped.FlatArray
import org.lasersonlab.zarr.utils.Idx
import org.lasersonlab.zarr.utils.Idx.Long.CastException
import org.lasersonlab.{ zarr ⇒ z }
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
   * This is necessary due to some unification limitations relating to the various aliases ([[Array.?]],
   * [[Array.Aux]], [[Array.??]], etc.) used to specify different subsets of an [[Array]]'s dependent types' that are
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

  val       shape: ShapeT[Dimension[Idx]]
  val chunkRanges: ShapeT[    Chunk.Idx ]

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
  type   T[                                    _T] = Array {                                                type T = _T }
  type Aux[_ShapeT[_], _Idx, _A[_], _Chunk[_], _T] = Array { type ShapeT[U] = _ShapeT[U]; type Idx = _Idx ; type T = _T ; type A[U] = _A[U]; type Chunk[U] = _Chunk[U] }
  type  Of[_ShapeT[_], _Idx,                   _T] = Array { type ShapeT[U] = _ShapeT[U]; type Idx = _Idx ; type T = _T }
  type   ?[_ShapeT[_], _Idx                      ] = Array { type ShapeT[U] = _ShapeT[U]; type Idx = _Idx }  // element-type unknown
  type  ??[            _Idx                      ] = Array { type ShapeT[U] =    List[U]; type Idx = _Idx }  // element-type and number of dimensions unknown

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
    Shape[_]
           : Traverse
           : Zip
           : Scannable,
      Idx,
       _T,
        A[_]
           : Traverse
  ](
      dir: Path,
    shape: Shape[Dimension[Idx]]
  )(
   implicit
      indices:    Indices.Aux[A, Shape],
          idx:        Idx.  T[     Idx],
     datatype:   DataType.Aux[      _T],
   compressor: Compressor
  ):
    Exception |
    A[
      Chunk[
        Shape,
        _T
      ]
    ]
  = {
    import idx._
    val sizeHint = shape.foldLeft(1) { _ * _.chunk }
    for {
      arr ←
        indices(
          shape
            .map { _.range }
        )
        .map {
          idx ⇒
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
   * Convenience-constructor: given [[ShapeT shape]]- and [[T data]]-types, load an [[Array]] from a [[Path directory]]
   *
   * Uses a [[VectorEvidence]] as evidence for mapping from the [[Nat]] to a concrete shape
   *
   * Differs from [[apply]] above in that it returns full-resolved [[Array.A]] and [[Array.Chunk]] type-members, for
   * situations where that is important (in general, it shouldn't be; tests may wish to verify / operate on chunks, but
   * users shouldn't ever need to).
   *
   * @param dir path to load as a Zarr [[Array]]
   * @param v implementation of [[ShapeT]]-type
   * @param idx "index" type to use (e.g. [[Int]] or [[Long]])
   * @param d datatype-decoder
   * @param dt fill-value-decoder
   * @tparam ShapeT "shape" of the [[Array]]; also the type of elements' indices
   * @tparam T element-type of this [[Array]]
   */
  def apply[
    ShapeT[_],
         T
  ](
    dir: Path
  )(
    implicit
      v:    VectorEvidence[ShapeT],
      d:  DataType.Decoder[     T],
     dt: FillValue.Decoder[     T],
    idx:               Idx
  ):
    Exception |
    Aux[
      ShapeT,
      idx.T,
      v.A,
      Chunk[ShapeT, ?],
      T
    ]
  = {
    import v._
    apply[ShapeT, A, T](dir)(
      // shouldn't have to list all these explicitly: https://github.com/scala/bug/issues/11086
                idx = idx,
                  d = d,
                 dt = dt,
           traverse = traverse,
                 ti = ti,
          arrayLike = arrayLike,
         shapeCodec = shapeCodec,
      traverseShape = traverseShape,
           zipShape = zipShape,
          scannable = scannable
    )
  }

  def apply[
    Shape[_],
        A[_],
        T
  ](
    dir: Path
  )(
    implicit
              idx:               Idx          ,
                d:  DataType.Decoder[       T],
               dt: FillValue.Decoder[       T],
         traverse:          Traverse[A       ],
               ti:       Indices.Aux[A, Shape],
        arrayLike:     ArrayLike.Aux[A, Shape],
       shapeCodec:            CodecK[   Shape],
    traverseShape:          Traverse[   Shape],
         zipShape:               Zip[   Shape],
        scannable:         Scannable[   Shape]
  ):
    Exception |
    Aux[
      Shape,
      idx.T,
      A,
      Chunk[Shape, ?],
      T
    ]
  = {
    import Idx.helpers.specify
    for {
      _metadata ←
        dir.load[
          Metadata[
            Shape,
            idx.T,
            T
          ]
        ]
      arr ← Array(dir, _metadata)
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
    idx: Idx
  ):
    Exception |
    Array.??[idx.T]
  =
    metadata.?(dir)
      .flatMap {
        metadata ⇒
          import Idx.helpers.specify
          apply[
            metadata.T,
            List,
            idx.T,
            FlatArray.*
          ](
            dir,
            metadata.t
          )
          .map {
            arr ⇒ arr: Array.??[idx.T]
          }
      }

  def apply[
        _T,
    _Shape[_]
            : Scannable
            : Zip
            : Traverse
            : EncoderK,
      _Idx,
        _A[_]
  ](
    dir: Path,
    _metadata: Metadata[_Shape, _Idx, _T]
  )(
    implicit
           ti:   Indices.Aux[_A, _Shape],
    arrayLike: ArrayLike.Aux[_A, _Shape],
     traverse:      Traverse[_A],
          idx:       Idx.  T[_Idx]
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
        type      T    =      _T
        type    Idx    =    _Idx
        type ShapeT[U] =  _Shape[U]
        type      A[U] =      _A[U]
        type  Chunk[U] = z.Chunk[ShapeT, U]

        val traverseA     = Traverse[     A]
        val traverseShape = Traverse[ShapeT]

        val foldableChunk = Chunk.foldable[ShapeT]

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
  ](
    implicit
    v: VectorEvidence[Shape]
  ):
    Load[
      lasersonlab.zarr.Array[Shape, T]
    ] =
  {
    implicit val int = Idx.Int
    loadArr[Shape, T]
  }

  implicit def loadArr[
    Shape[_],
    T
      :  DataType.Decoder
      : FillValue.Decoder
  ](
    implicit
      v: VectorEvidence[Shape],
    idx: Idx
  ):
    Load[
      Of[Shape, idx.T, T]
    ] =
    new Load[Of[Shape, idx.T, T]] {
      override def apply(dir: Path): Exception | Of[Shape, idx.T, T] =
        Array[Shape, T](dir)
    }

  implicit def saveUntyped[
    Shape[_]
           : EncoderK
           : Scannable
  ](
    implicit
    idx: Idx
  ):
    Save[
      Array.?[Shape, idx.T]
    ] =
    new Save[
      Array.?[
        Shape,
        idx.T
      ]
    ] {
      def apply(
        a: Array.?[Shape, idx.T],
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
            .sequence//[Throwable | ?, Unit]
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
