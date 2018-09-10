package org.lasersonlab.zarr

import cats.{ Eval, Foldable, Traverse }
import hammerlab.option._
import hammerlab.path._
import _root_.io.circe.{ Decoder, Encoder }
import org.lasersonlab.ndarray.{ Arithmetic, ArrayLike, ScanRight, Sum }
import org.lasersonlab.zarr
import org.lasersonlab.zarr.FillValue.{ FillValueDecoder, FillValueEncoder }
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.{ Load, Save }
import shapeless.Nat

import scala.util.Try

/**
 * A Zarr N-dimensional array
 *
 * Storage of the ND-array of chunks, as well as the records in each chunk, are each a configurable type-param; see
 * companion-object for some convenient constructors
 */
trait Array[T] {
  type Shape
  type A[_]
  type Chunk[_]

  implicit def traverseA: Traverse[A]
  implicit def foldableChunk: Foldable[Chunk]

  def chunksIterator: Iterator[Chunk[T]] =
    traverseA
      .toList(chunks)
      .iterator

  def elems: Iterator[T] =
    for {
      chunk ← chunksIterator
      elem ← foldableChunk.toList(chunk).iterator
    } yield
      elem

  def shape: Shape
  def chunkShape: Shape

  def apply(idx: Shape): T

  def metadata: Metadata[T, Shape]
  def chunks: A[Chunk[T]]

  // TODO: this should be type-parameterizable, and validated accordingly during JSON-parsing
  def attrs: Opt[Attrs]

  def save(dir: Path): Throwable | Unit
}

object Array {

  type S[S, T] = Array[T] { type Shape = S }

  type Aux[S, _A[_], _Chunk[_], T] =
    Array[T] {
      type Shape = S
      type     A[U] =     _A[U]
      type Chunk[U] = _Chunk[U]
    }

  def unapply[T](a: Array[T]):
    Option[
      (
        Metadata[T, a.Shape],
        Option[Attrs],
        a.A[a.Chunk[T]]
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
   * Each chunk lives in a file with basename given by the provided [[Key]] ('.'-joined indices)
   */
  private def chunks[
        T: DataType.Aux,
    Shape: Arithmetic.Id,
     A[U]: Traverse
  ](
           dir: Path,
      arrShape: Shape,
    chunkShape: Shape
  )(
     implicit
     indices: Indices.Aux[A, Shape],
     ai: Arithmetic[Shape, Int],
     key: Key[Shape],
     scanRight: ScanRight.Aux[Shape, Int, Int, Shape],
     sum: Sum.Aux[Shape, Int],
     compressor: Compressor,
  ):
    Exception |
    A[Chunk[Shape, T]]
  = {

    val chunkRanges = (arrShape + chunkShape - 1) / chunkShape

    // We use Traverse and Applicative instances for Either, Traverse[A], and Functor syntax
    import cats.implicits._

    val chunks =
      indices(chunkRanges)
        .map {
            idx ⇒
              val start = idx * chunkShape
              val end = arrShape min ((idx + 1) * chunkShape)

              // chunks in the last "row" of any dimension may be smaller
              val shape = end - start

              Chunk(
                dir / key(idx),
                shape,
                idx,
                start,
                end,
                compressor
              )(
                chunkShape
              )
          }

    // A[Err | Chunk] -> Err | A[Chunk]
    chunks
      .sequence[
        Exception | ?,
        Chunk[Shape, T]
      ]
  }

  /**
   * Convenience-constructor: given a data-type and a [[Nat (type-level) number of dimensions]], load an [[Array]] from
   * a [[Path directory]]
   *
   * Uses a [[VectorInts]] as evidence for mapping from the [[Nat]] to a concrete shape
   */
  def apply[
    T,
    N <: Nat
  ](
    dir: Path
  )(
    implicit
    v: VectorInts[N],
    d: Decoder[DataType.Aux[T]],
    e: Encoder[DataType.Aux[T]],
    dt: FillValueDecoder[T],
    et: FillValueEncoder[T]
  ):
    Exception |
    S[v.Shape, T]
  =
    chunks[
      T,
      N
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
    N <: Nat
  ](
    dir: Path
  )(
    implicit
    v: VectorInts[N],
    d: Decoder[DataType.Aux[T]],
    e: Encoder[DataType.Aux[T]],
    dt: FillValueDecoder[T],
    et: FillValueEncoder[T]
  ):
    Exception |
    Aux[v.Shape, v.A, Chunk[v.Shape, ?], T]
  = {
    import v._
    apply[T, v.Shape, v.A](dir)(
      // shouldn't have to list all these explicitly: https://github.com/scala/bug/issues/11086
      d = d,
      e = e,
      ti = ti,
      traverse = traverse,
      arrayLike = arrayLike,
      ai = ai,
      scanRight = scanRight,
      sum = sum,
      dt = dt,
      et = et,
      arithmetic = arithmetic,
      key = key,
      ds = ds,
      es = es
    )
  }

  def apply[
    T,
    _Shape,
    _A[U]
  ](
    dir: Path
  )(
    implicit
    d: Decoder[DataType.Aux[T]],
    e: Encoder[DataType.Aux[T]],
    ti: Indices.Aux[_A, _Shape],
    traverse: Traverse[_A],
    arrayLike: ArrayLike.Aux[_A, _Shape],
    ai: Arithmetic[_Shape, Int],
    scanRight: ScanRight.Aux[_Shape, Int, Int, _Shape],
    sum: Sum.Aux[_Shape, Int],
    dt: FillValueDecoder[T],
    et: FillValueEncoder[T],
    arithmetic: Arithmetic.Id[_Shape],
    key: Key[_Shape],
    ds: Decoder[_Shape],
    es: Encoder[_Shape]
  ):
    Exception |
    Aux[_Shape, _A, Chunk[_Shape, ?], T]  // TODO: replace Bytes with something lazy / network-based
  =
    for {
      _metadata ← dir.load[Metadata[T, _Shape]]
         _attrs ← dir.load[Opt[Attrs]]
      _chunks ← {
        implicit val md = _metadata
        import Metadata._
        chunks[T, _Shape, _A](
          dir,
          md.shape,
          md.chunks
        )
      }
    } yield
      new Array[T] {
        type Shape = _Shape
        type A[U] = _A[U]
        type Chunk[U] = zarr.Chunk[Shape, U]

        override val traverseA = traverse
        override val foldableChunk = Chunk.foldable

        val metadata = _metadata
        val   chunks =   _chunks
        val    attrs =    _attrs

        val shape = metadata.shape
        val chunkShape = metadata.chunks

        def apply(idx: Shape): T =
          Chunk.arrayLike(
            arrayLike(
              chunks,
              idx / chunkShape
            ),
            idx % chunkShape
          )

        def save(dir: Path): Throwable | Unit = {
          import cats.implicits._

          val chunkRanges = (shape + chunkShape - 1) / chunkShape

          def chunkResults: Throwable | Unit = {
            ti(chunkRanges)
              .map {
                idx ⇒
                  val chunk = arrayLike(chunks, idx)
                  val path = dir / key(idx)
                  Try {
                    import java.nio.ByteBuffer._
                    val datatype = metadata.dtype
                    val buffer = allocate(datatype.size * chunk.size)
                    chunk.foldLeft[Unit](()) {
                      (_, elem) ⇒
                        datatype(buffer, elem)

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

          for {
            _ ← metadata.save(dir)
            _ ← attrs.save(dir)
            _ ← chunkResults
          } yield
            ()
        }
      }

  import cats.implicits._

  implicit def foldable[Shape]: Foldable[Array.S[Shape, ?]] =
    new Foldable[S[Shape, ?]] {
      type F[A] = S[Shape, A]

      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B =
        fa
          .traverseA
          .foldLeft(
            fa.chunks,
            b
          ) {
            (b, chunk) ⇒
              fa
                .foldableChunk
                .foldLeft(
                  chunk,
                  b
                )(
                  f
                )
          }

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] =
        fa
          .traverseA
          .foldRight(
            fa.chunks,
            lb
          ) {
            (chunk, lb) ⇒
              fa
                .foldableChunk
                .foldRight(
                  chunk,
                  lb
                )(
                  f
                )
          }
    }

  implicit def foldableDerived[T](implicit a: Array[T]): Foldable[Aux[a.Shape, a.A, a.Chunk, ?]] =
    new Foldable[Aux[a.Shape, a.A, a.Chunk, ?]] {
      type F[A] = Aux[a.Shape, a.A, a.Chunk, A]

      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) ⇒ B): B =
        fa
          .traverseA
          .foldLeft(
            fa.chunks,
            b
          ) {
            (b, chunk) ⇒
              fa
                .foldableChunk
                .foldLeft(
                  chunk,
                  b
                )(
                  f
                )
          }

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] =
        fa
          .traverseA
          .foldRight(
            fa.chunks,
            lb
          ) {
            (chunk, lb) ⇒
              fa
                .foldableChunk
                .foldRight(
                  chunk,
                  lb
                )(
                  f
                )
          }
    }

  implicit def load[T, N <: Nat, Shape](
    implicit
    v: VectorInts.Ax[N, Shape],
    d: Decoder[DataType.Aux[T]],
    e: Encoder[DataType.Aux[T]],
    dt: FillValueDecoder[T],
    et: FillValueEncoder[T]
  ):
    Load[
      S[Shape, T]
    ] =
    new Load[S[Shape, T]] {
      override def apply(dir: Path): Exception | S[Shape, T] =
        Array[T, N](dir)
    }

  implicit def save[
    T,
    Shape
  ]:
    Save[
      S[Shape, T]
    ] =
    new Save[S[Shape, T]] {
      def apply(t: S[Shape, T], dir: Path): Throwable | Unit = t.save(dir)
    }
}
