package org.lasersonlab.zarr

import cats.data.Nested
import cats.implicits._
import cats.{ Applicative, Eval, Foldable, Traverse }
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.circe.{ Codec, CodecK, DecoderK, EncoderK }
import org.lasersonlab.ndarray.ArrayLike
import org.lasersonlab.shapeless.{ Scannable, Zip }
import org.lasersonlab.zarr
import org.lasersonlab.zarr.circe.{ Decoder, Encoder }
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
  type T
  type ShapeT[_]
  type Idx
  type Shape = ShapeT[Idx]
  type Index = Shape
  type A[_]
  type Chunk[_]

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

  /**
   * Random-indexing operation
   */
  def apply(idx: Index): T  // TODO: support exceptions: IndexOutOfBounds, IO, etc.

  val metadata: Metadata[ShapeT, Idx, T]

  /**
   * All the [[Array]]'s data lives here: an N-dimensional array of [[Chunk chunks]] that contain elements of type [[T]]
   */
  val chunks: A[Chunk[T]]

  // TODO: this should be type-parameterizable, and validated accordingly during JSON-parsing
  val attrs: Opt[Attrs]

  def save(dir: Path): Throwable | Unit

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

  type     Aux[_ShapeT[_], _Idx, _A[_], _Chunk[_], _T] = Array { type ShapeT[U] =    _ShapeT[U]; type Idx = _Idx; type T = _T; type A[U] = _A[U]; type Chunk[U] = _Chunk[U] }
  type      Of[_ShapeT[_], _Idx,                   _T] = Array { type ShapeT[U] =    _ShapeT[U]; type Idx = _Idx; type T = _T }
  type Untyped[_ShapeT[_], _Idx                      ] = Array { type ShapeT[U] =    _ShapeT[U]; type Idx = _Idx }
  type    List[            _Idx                      ] = Array { type ShapeT[U] = scala.List[U]; type Idx = _Idx }

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
        _T,
    Shape[_]: Traverse : Zip : Scannable,
      Idx,
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

    for {
      chunkRanges ←
        shape
          .map {
            case Dimension(arr, chunk) ⇒
              int {
                (arr + chunk - 1) / chunk
              }
          }
          .sequence[CastException | ?, Chunk.Idx]

      sizeHint = shape.foldLeft(1) { _ * _.chunk }

      arr ← {
        val chunks =
          indices(chunkRanges)
            .map {
              idx: Shape[Chunk.Idx] ⇒
                import Zip.Ops
                for {
                  chunkShape ←
                    // chunks in the last "row" of any dimension may be smaller
                    shape
                      .zip(idx)
                      .map {
                        case (Dimension(arr, chunk), idx) ⇒
                          val start = idx * chunk
                          val end = arr min ((idx + 1) * chunk)

                          int { end - start }
                      }
                      .sequence[
                        CastException | ?,
                        Chunk.Idx
                      ]

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

        // A[Err | Chunk] -> Err | A[Chunk]
        chunks
          .sequence[
            Exception | ?,
            Chunk[Shape, _T]
          ]
      }
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
  def apply[
    T,
    N <: Nat,
    Idx  // TODO: move this to implicit evidence
  ](
    dir: Path
  )(
    implicit
     v: VectorEvidence[N, Idx],
     d:  DataType.Decoder[T],
     e:  DataType.Encoder[T],
    dt: FillValue.Decoder[T],
    et: FillValue.Encoder[T]
  ):
    Exception |
    Aux[v.ShapeT, Idx, v.A, Chunk[v.ShapeT, ?], T]
  = {
    import v._
    apply[T, v.ShapeT, Idx, v.A](dir)(
      // shouldn't have to list all these explicitly: https://github.com/scala/bug/issues/11086
      d = d,
      e = e,
      ti = ti,
      traverse = traverse,
      arrayLike = arrayLike,
      dt = dt,
      et = et,
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
    Idx,
    _A[_]
  ](
    dir: Path
  )(
    implicit
    d: DataType.Decoder[_T],
    e: DataType.Encoder[_T],
    ti: Indices.Aux[_A, _Shape],
    traverse: Traverse[_A],
    arrayLike: ArrayLike.Aux[_A, _Shape],
    dt: FillValue.Decoder[_T],
    et: FillValue.Encoder[_T],
    shapeCodec: CodecK[_Shape],
    idx: Idx.T[Idx],
    traverseShape: Traverse[_Shape],
    zipShape: Zip[_Shape],
    scannable: Scannable[_Shape]
  ):
    Exception |
    Aux[
      _Shape,
      Idx,
      _A,
      Chunk[
        _Shape,
        ?
      ],
      _T
    ]
  =
    for {
      _metadata ← dir.load[Metadata[_Shape, Idx, _T]]
      arr ← Array(dir, _metadata)
    } yield
      arr

  def untyped[Idx](dir: Path)(implicit idx: Idx.T[Idx]): Exception | Array.List[Idx] =
    zarr.array.metadata.untyped(dir)
      .flatMap {
        metadata ⇒
          apply[
            metadata.T,
            scala.List,
            Idx,
            FlatArray
          ](
            dir,
            metadata.t
          )
          .map {
            arr ⇒ arr: List[Idx]
          }
      }

  def apply[
    _T,
    _Shape[_]: Scannable : Zip,
    _Idx,
    _A[_]
  ](
    dir: Path,
    _metadata: Metadata[_Shape, _Idx, _T]
  )(
    implicit
    e: Encoder[DataType.Aux[_T]],
    ti: Indices.Aux[_A, _Shape],
    traverse: Traverse[_A],
    _traverseShape: Traverse[_Shape],
    arrayLike: ArrayLike.Aux[_A, _Shape],
    et: FillValue.Encoder[_T],
    senc: EncoderK[_Shape],
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
        chunks[_T, _Shape, _Idx, _A](
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

        val traverseA = traverse
        val foldableChunk = Chunk.foldable
        val traverseShape = _traverseShape

        import idx.{ arithmeticInt, encoder, int }

        val metadata = _metadata
        val datatype = metadata.dtype
        val   chunks =   _chunks
        val    attrs =    _attrs

        val shape = metadata.shape

        val chunkRanges =
          shape
            .map {
              case Dimension(arr, chunk) ⇒
                int {
                  (arr + chunk - 1) / chunk
                }
            }
            .sequence
            .right
            .get

        import lasersonlab.shapeless.slist._

        def apply(idx: Shape): T = {
          import Zip.Ops

          // aliases for annotating the `.sequence` shenanigans below
          type E[U] = CastException | U
          type F[U] = ShapeT[U]
          type G[U] = `2`[U]
          type T = Chunk.Idx

          val chunkIdx :: offset :: ⊥ =  // ShapeT[Int] :: ShapeT[Int]
            Nested(
              idx
                .zip(shape)
                .map {
                  case (idx, Dimension(_, chunk)) ⇒
                    int { idx / chunk } ::
                    int { idx % chunk } ::
                    ⊥
                }                    : F[G[E[T]]]
            )
            .sequence               // E[F[G[T]]]
            .map(_.value.sequence)  // E[G[F[T]]]
            .right
            .get                     :   G[F[T]]

          arrayLike(
            chunks,
            chunkIdx
          )(
            offset
          )
        }

        // TODO: move this implementation into base trait, or separate Save instance
        def save(dir: Path): Throwable | Unit = {

          def chunkResults: Throwable | Unit = {
            ti(chunkRanges)
              .map {
                idx ⇒
                  val chunk: Chunk[_T] = arrayLike(chunks, idx)
                  val path = dir / Key(idx)
                  Try {
                    import java.nio.ByteBuffer._
                    val buffer = allocate(datatype.size * chunk.size)
                    chunk.foldLeft(()) {
                      (_, elem) ⇒
                        datatype(buffer, elem: _T)

                        ()
                    }

                    val os =
                      metadata.compressor(
                        path.outputStream(mkdirs = true),
                        datatype.size
                      )

                    os.write(buffer.array())
                    os.close()
                  }
                  .toEither
              }
              .sequence
              .map { _ ⇒ () }
          }

          // TODO: configure ability to write to a temporary location and then "commit" all results
          for {
            _ ← _metadata.save(dir)
            _ ← attrs.save(dir)
            _ ← chunkResults
          } yield
            ()
        }
      }

  type T[_T] = Array { type T = _T }

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
  implicit def loadArrInt[Shape[_], T, N <: Nat](
    implicit
     v: VectorEvidence.Ax[N, Shape, Int],
     d: DataType.Decoder[T],
     e: DataType.Encoder[T],
    dt: FillValue.Decoder[T],
    et: FillValue.Encoder[T]
  ):
    Load[
      lasersonlab.zarr.Array[Shape, T]
    ] =
    loadArr[T, N, Int, Shape]

  // TODO: canonicalize type-param ordering
  implicit def loadArr[T, N <: Nat, Idx, Shape[_]](
    implicit
     v: VectorEvidence.Ax[N, Shape, Idx],
     d: DataType.Decoder[T],
     e: DataType.Encoder[T],
    dt: FillValue.Decoder[T],
    et: FillValue.Encoder[T]
  ):
    Load[
      Of[Shape, Idx, T]
    ] =
    new Load[Of[Shape, Idx, T]] {
      override def apply(dir: Path): Exception | Of[Shape, Idx, T] =
        Array[T, N, Idx](dir)
    }

  implicit def save[
    T,
    Shape[_],
    Idx
  ]:
    Save[
      Of[Shape, Idx, T]
    ] =
    new Save[Of[Shape, Idx, T]] {
      def apply(t: Of[Shape, Idx, T], dir: Path): Throwable | Unit = t.save(dir)
    }
}
