package org.lasersonlab.zarr

import cats.{ Eval, Foldable, Traverse }
import org.lasersonlab.circe.EncoderK
import org.lasersonlab.ndarray.Vector
import org.lasersonlab.slist.Scannable
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.io.{ Load, Save }
import org.lasersonlab.zarr.utils.Idx

import scala.util.Try

/**
 * A Zarr N-dimensional array
 *
 * Storage of the ND-array of chunks, as well as the records in each chunk, are each a configurable type-param; see
 * companion-object for some convenient constructors
 *
 * Two constructors are provided in the companion object:
 *
 * - one that reads an [[Array]] from a [[Path directory path]]
 * - one that takes the elements (as well as shape and other metadata) as arguments
 *
 * The `convert` module contains yet another, which loads from an HDF5 file.
 *
 * TODO: experiment with Breeze vector/array for 1D/2D cases
 * TODO: auto-[[Save]] [[Vector]]s (utilizing an implicit chunk-size in bytes)
 * TODO: add a sensible toString
 */
sealed trait Array {
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
   * - [[Chunk chunks]] are [[Foldable]]
   */
  implicit val traverseA    : Traverse[     A]
  implicit val traverseShape: Traverse[ShapeT]
  implicit val foldableChunk: Foldable[ Chunk]

  /**
   * Widen to an [[Array.T]], so that [[cats]] typeclasses (e.g. [[Array.foldableT]]) can be picked up, and
   * corresponding syntax used, e.g.
   *
   * {{{
   * arr.t.toList
   * }}}
   *
   * This is necessary due to some unification limitations relating to the various aliases ([[Array.?]],
   * [[Array.Aux]], [[Array.*?]], etc.) used to specify different subsets of an [[Array]]'s dependent types' that are
   * known at a given call-site
   */
  def t: Array.T[T] = this match { case a: Array.T[T] ⇒ a }

  /**
   * Short-hand for imbuing this [[Array]] with an element type at runtime, e.g. in the case where it was loaded without
   * that type having been known ahead of time
   */
  def as[T]: Array.Aux[this.ShapeT, this.Idx, this.A, this.Chunk, T] =
    this
      .asInstanceOf[
        Array.Aux[
          this.ShapeT,
          this.Idx,
          this.A,
          this.Chunk,
          T
        ]
      ]

  val       shape: ShapeT[Dimension[Idx]]
  def chunkRanges: ShapeT[    Chunk.Idx ] = shape.map { _.range }

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
  val attrs: Option[Attrs]

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

object Array
  extends array.load
     with array.make
{

  /**
   * "Aux" aliases for various subsets of [[Array]]'s type-members that might be known / required in various contexts
   *
   * Everything is only instantiable via [[Aux]] to users outside this class.
   */
  sealed trait   T[                  _T] extends Array { type T = _T }                                   // element-type known, shape unknown
  sealed trait   ?[_ShapeT[_], _Idx    ] extends Array { type ShapeT[U] = _ShapeT[U]; type Idx = _Idx }  // element-type unknown, shape known
  sealed trait  Of[ ShapeT[_],  Idx, _T] extends ?[ShapeT,  Idx] with T[_T]
          type  *?[            _Idx    ] =       ?[  List, _Idx]                                         // element-type and number of dimensions unknown

  abstract class Aux[
    ShapeT[_],
       Idx   ,
        _A[_],
    _Chunk[_],
         T
  ]
  extends Of[ShapeT, Idx, T] {
    type     A[U] =     _A[U]
    type Chunk[U] = _Chunk[U]
  }

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

  implicit def loadArr[
    Shape[_]
    : VectorEvidence,
    T
    :  DataType.Decoder
    : FillValue.Decoder
  ](
    implicit
    idx: Idx
  ):
    Load[
      Of[Shape, idx.T, T]
    ] =
    new Load[Of[Shape, idx.T, T]] {
      def apply(dir: Path): Exception | Of[Shape, idx.T, T] = Array[Shape, T](dir)
    }

  /**
   * Wrapper for [[save_?]] below; [[Array.?]] is basically the least-common-ancestor [[Array]]-type that common
   * [[Save "save"]] logic can be implemented for, but some unification limitations mean that [[Array.Of]]-subtypes and
   * aliases don't discover that they are [[Save "save"-able]] via [[save_?]] without this wrapper helping.
   */
  implicit def saveOf[
    ShapeT[_]
    : EncoderK
    : Scannable,
    T
  ](
    implicit
    idx: Idx
  ):
    Save[
      Of[ShapeT, idx.T, T]
    ] =
    Save.as(a ⇒ a: ?[ShapeT, idx.T])

  implicit def save_?[
    _ShapeT[_]
    : EncoderK
    : Scannable,
    _Idx: Idx.T
  ]:
  Save[
    Array.?[_ShapeT, _Idx]
  ] =
    new Save[Array.?[_ShapeT, _Idx]] {
      def direct(
        _a: Array.?[_ShapeT, _Idx],
        dir: Path
      ):
        Throwable |
        Unit
      = {
        // work around https://github.com/scala/bug/issues/11086; method-params incorrectly considered "unstable"
        val a = _a
        import a._

        def chunkResults: Throwable | Unit = {
          val (_, chunkStrides) = chunkRanges.scanRight(1)(_ * _)
          val chunkSize =
            shape
              .foldLeft(1) {
                _ * _.chunk
              }

          a
            .chunks
            .mapWithIndex {
              (chunk, int) ⇒
                // traverse chunk-strides to convert linear/integer index to structured N-dimensional index
                val idx =
                  chunkStrides
                    .scanLeft_→(
                      (
                        int,  // output
                        int   // remaining
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
            .sequence
            .map { _ ⇒ () }
        }

        for {
          _ ← a.metadata.save(dir)
          _ ← a.   attrs.save(dir)
          _ ← chunkResults
        } yield
          ()
      }
    }
}
