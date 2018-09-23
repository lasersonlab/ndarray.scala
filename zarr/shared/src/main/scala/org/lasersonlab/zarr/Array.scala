package org.lasersonlab.zarr

import cats.implicits._
import cats.{ Applicative, Eval, Foldable, Monad, Semigroupal, Traverse }
import hammerlab.option._
import hammerlab.path._
import org.lasersonlab.ndarray.{ ArrayLike, Scannable }
import org.lasersonlab.zarr
import org.lasersonlab.zarr.circe.{ Decoder, Encoder }
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.{ Basename, Load, Save }
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
  type A[_]
  type Chunk[_]

  implicit val traverseA: Traverse[A]
  implicit val traverseShape: Traverse[ShapeT]
  implicit val foldableChunk: Foldable[Chunk]

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

  def apply(idx: Shape): T

  val metadata: Metadata[T, ShapeT, Idx]

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

  type T[_T] = Array { type T = _T }
  type S [S[_], I, _T] = Array { type ShapeT[U] = S[U]; type Idx = I; type T = _T }
  type SU[S[_], I    ] = Array { type ShapeT[U] = S[U]; type Idx = I              }

  type Idxs[I] = Array { type ShapeT[U] = List[U]; type Idx =   I }
  type Ints    = Array { type ShapeT[U] = List[U]; type Idx = Int }
  type    L    = Array { type ShapeT[U] = List[U]                 }

  type Aux[S[_], I, _A[_], _Chunk[_], _T] =
    Array {
      type T = _T
      type ShapeT[U] = S[U]
      type Idx = I
      type     A[U] =     _A[U]
      type Chunk[U] = _Chunk[U]
    }

  def unapply(a: Array):
    Option[
      (
        Metadata[a.T, a.ShapeT, a.Idx],
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

  import Chunk.Idx

  /**
   * Load an ND-array of chunks from a [[Path directory]]
   *
   * Each chunk lives in a file with basename given by '.'-joined indices
   */
  private def chunks[
        _T,
    Shape[_]: Traverse : Semigroupal : Scannable,
      Idx,
        A[_]: Traverse
  ](
           dir: Path,
         shape: Shape[Dimension[Idx]]
  )(
   implicit
   indices: Indices.Aux[A, Shape[Chunk.Idx]],
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
    import Idx.Ops
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
                for {
                  chunkShape ←
                    // chunks in the last "row" of any dimension may be smaller
                    shape
                      .product(idx)
                      .map {
                        case (Dimension(arr, chunk), idx) ⇒
                          val start = idx * chunk
                          val end = arr min ((idx + 1) * chunk)

                          (end - start).int
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
   * Uses a [[VectorInts]] as evidence for mapping from the [[Nat]] to a concrete shape
   */
  def apply[
    T,
    N <: Nat,
    Idx: Idx.T
  ](
    dir: Path
  )(
    implicit
     v: VectorInts[N, Idx],
     d: Decoder[DataType.Aux[T]],
     e: Encoder[DataType.Aux[T]],
    dt: FillValue.Decoder[T],
    et: FillValue.Encoder[T]
  ):
    Exception |
    S[v.ShapeT, Idx, T]
  =
    chunks[
      T,
      N,
      Idx
    ](
      dir
    )

  /**
   * Convenience-constructor: given a data-type and a [[Nat (type-level) number of dimensions]], load an [[Array]] from
   * a [[Path directory]]
   *
   * Uses a [[VectorInts]] as evidence for mapping from the [[Nat]] to a concrete shape
   *
   * Differs from [[apply]] above in that it returns full-resolved [[Array.A]] and [[Array.Chunk]] type-members, for
   * situations where that is important (in general, it shouldn't be; tests may wish to verify / operate on chunks, but
   * users shouldn't ever need to).
   */
  def chunks[
    T,
    N <: Nat,
    Idx
  ](
    dir: Path
  )(
    implicit
     v: VectorInts[N, Idx],
     d: Decoder[DataType.Aux[T]],
     e: Encoder[DataType.Aux[T]],
    dt: FillValue.Decoder[T],
    et: FillValue.Encoder[T],
    idx: Idx.T[Idx]
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
      ds = ds,
      cds = cds,
      es = es,
      ces = ces,
      idx = idx,
      traverseShape = traverseShape,
      semigroupalShape = semigroupalShape,
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
    d: Decoder[DataType.Aux[_T]],
    e: Encoder[DataType.Aux[_T]],
    ti: Indices.Aux[_A, _Shape[Chunk.Idx]],
    traverse: Traverse[_A],
    arrayLike: ArrayLike.Aux[_A, _Shape[Chunk.Idx]],
    dt: FillValue.Decoder[_T],
    et: FillValue.Encoder[_T],
    ds: Decoder[_Shape[Idx]],
    cds: Decoder[_Shape[Chunk.Idx]],
    es: Encoder[_Shape[Idx]],
    ces: Encoder[_Shape[Chunk.Idx]],
    idx: Idx.T[Idx],
    traverseShape: Traverse[_Shape],
    semigroupalShape: Semigroupal[_Shape],
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
  = for {
      _metadata ← dir.load[Metadata[_T, _Shape, Idx]]
      arr ← Array(dir, _metadata)
    } yield
      arr

  def untyped[Idx](dir: Path)(implicit idx: Idx.T[Idx]): Exception | Idxs[Idx] =
    zarr.untyped.Metadata(dir)
      .flatMap {
        metadata ⇒
          val cc =
            new Metadata[metadata.T, List, Idx](
              shape = metadata.shape.toList,
              dtype = metadata.dtype,
              compressor = metadata.compressor,
              order = metadata.order,
              fill_value = metadata.fill_value,
              zarr_format = metadata.zarr_format,
              filters = metadata.filters
            )

          import idx.encoder

          apply[
            metadata.T,
            List,
            Idx,
            FlatArray
          ](
            dir,
            cc
          )
          .map {
            arr ⇒ arr: Idxs[Idx]
          }
      }

  def apply[
    _T,
    _Shape[_]: Semigroupal : Scannable,
    _Idx,
    _A[_]
  ](
    dir: Path,
    _metadata: Metadata[_T, _Shape, _Idx]
  )(
    implicit
    e: Encoder[DataType.Aux[_T]],
    ti: Indices.Aux[_A, _Shape[Idx]],
    traverse: Traverse[_A],
    _traverseShape: Traverse[_Shape],
    arrayLike: ArrayLike.Aux[_A, _Shape[Idx]],
    et: FillValue.Encoder[_T],
    es: Encoder[_Shape[_Idx]],
    ces: Encoder[_Shape[Idx]],
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

        import idx.{ arithmeticInt, int, encoder }

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

        type T2[E] = (E, E)
        implicit val appT2: Applicative[T2] = ???
        implicit val trvT2: Traverse[T2] = ???
        implicit val eappT2: Applicative[λ[U ⇒ CastException | T2[U]]] = ???

        def apply(idx: Shape): T = {
          val (
            chunkIdx,
            offset
          ) =
            idx
              .product(shape)
              .map {
                case (idx, Dimension(_, chunk)) ⇒
                  (
                    (
                      int { idx / chunk },
                      int { idx % chunk }
                    ):
                    T2[CastException | Chunk.Idx]
                  )
                  .sequence[CastException | ?, Chunk.Idx]
              }
              .sequence[λ[U ⇒ CastException | T2[U]], Chunk.Idx]
              .right
              .get

          arrayLike(
            chunks,
            chunkIdx
          )(
            offset
          )
        }

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

  /**
   * Implement [[Foldable]] on an [[Array]] identified only by its element type; part of working around / mitigating
   * https://github.com/scala/bug/issues/11169.
   *
   * [[Array.foldLeft foldLeft]] and [[Array.foldRight foldRight]] are defined directly on [[Array]], but this can still
   * be useful in contexts based around Cats typeclasses
   */
  implicit val foldableT: Foldable[Array.T] =
    new Foldable[Array.T] {
      override def foldLeft[A, B](fa: T[A], b: B)(f: (B, A) ⇒ B): B = ???
      override def foldRight[A, B](fa: T[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
    }

  /**
   * Implement [[Foldable]] on an [[Array]] by making it implicit; part of working around / mitigating
   * https://github.com/scala/bug/issues/11169
   *
   * [[Array.foldLeft foldLeft]] and [[Array.foldRight foldRight]] are defined directly on [[Array]], but this can still
   * be useful in contexts based around Cats typeclasses
   */
  implicit def foldableDerived[T](implicit a: Array.T[T]): Foldable[Aux[a.ShapeT, a.Idx, a.A, a.Chunk, ?]] =
    new Foldable[Aux[a.ShapeT, a.Idx, a.A, a.Chunk, ?]] {
      type F[A] = Aux[a.ShapeT, a.Idx, a.A, a.Chunk, A]
      @inline def foldLeft [A, B](fa: F[A],  b:      B )(f: (B,      A ) ⇒      B ):      B  = fa.foldLeft ( b)(f)
      @inline def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = fa.foldRight(lb)(f)
    }

  // TODO: type-parameterize Idx
  implicit def loadArr[T, N <: Nat, Shape[_]](
    implicit
    v: VectorInts.Ax[N, Shape, Int],
    d: Decoder[DataType.Aux[T]],
    e: Encoder[DataType.Aux[T]],
    dt: FillValue.Decoder[T],
    et: FillValue.Encoder[T]
  ):
    Load[
      S[Shape, Int, T]
    ] =
    new Load[S[Shape, Int, T]] {
      override def apply(dir: Path): Exception | S[Shape, Int, T] =
        ???
        //Array[T, N, Int](dir)
    }

  implicit def save[
    T,
    Shape[_],
    Idx
  ]:
    Save[
      S[Shape, Idx, T]
    ] =
    new Save[S[Shape, Idx, T]] {
      def apply(t: S[Shape, Idx, T], dir: Path): Throwable | Unit = t.save(dir)
    }
}
