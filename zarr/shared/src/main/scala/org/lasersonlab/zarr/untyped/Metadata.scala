package org.lasersonlab.zarr.untyped

import hammerlab.option._
import hammerlab.path._
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import org.lasersonlab.zarr
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.FillValue.Null
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr.Metadata._
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr._
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.utils.Idx

// TODO: seal this, put it in same file as "typed" metadata
trait Metadata {
  val shape: Shape[Dimension[Idx]]
  val dtype: DataType
  val compressor: Compressor = Blosc()
  val order: Order = C
  val fill_value: FillValue[T] = Null
  val zarr_format: Format = `2`
  val filters: Opt[Seq[Filter]] = None

  type T = dtype.T
  type Shape[_]
  type Idx

  /** Allows a caller to coerce the type of a [[Metadata]] to include its constituent types */
  def t = this.asInstanceOf[zarr.Metadata[T, Shape, Idx]]
}

object Metadata {

  // TODO: simplify/rename these aliases
  type T[_T, _I] =
    Metadata {
      type T = _T
      type Idx = _I
    }

  type S[_S[_], _I] =
    Metadata {
      type Shape[U] = _S[U]
      type Idx = _I
    }

  type Aux[_T, _S, _I] =
    Metadata {
      type T = _T
      type Shape = _S
      type Idx = _I
    }

  def apply(dir: Path)(implicit idx: Idx): Exception | S[List, idx.T] =
    dir ? basename flatMap {
      path ⇒
        decode[
          S[
            List,
            idx.T
          ]
        ](
          path.read
        )
    }

  implicit def decoder(
    implicit
    idx: Idx
  ):
    Decoder[
      S[
        List,
        idx.T
      ]
    ] =
    new Decoder[S[List, idx.T]] {
      type Idx = idx.T
      def apply(c: HCursor): Result[S[List, Idx]] = {
        for {
                _shape ← c.downField(      "shape").as[List[Idx]]
               _chunks ← c.downField(     "chunks").as[List[Chunk.Idx]]
                _dtype ← c.downField(      "dtype").as[DataType]
           _compressor ← c.downField( "compressor").as[Compressor]
                _order ← c.downField(      "order").as[Order]
          _zarr_format ← c.downField("zarr_format").as[Format]
        } yield
          if (_shape.size == _chunks.size)
            new zarr.Metadata[_dtype.T, List, Idx](
              _shape
                .zip(_chunks)
                .map {
                  case (arr, chunk) ⇒
                    Dimension(arr, chunk)
                },
              _dtype,
              _compressor,
              _order,
              // TODO: move fill-value parsing into datatype
              FillValue.Null,
              _zarr_format,
              c
                .downField("filters")
                .as[Seq[Filter]]
                .toOption
            )
          else
            throw new IllegalStateException(
              s"Shape and chunks arrays have unequal sizes (${_shape.size} vs ${_chunks.size}): ${_shape.mkString(",")} ${_chunks.mkString(",")}"
            )
      }
    }
}
