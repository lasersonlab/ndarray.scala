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
import org.lasersonlab.zarr.io.Basename
import org.lasersonlab.zarr.utils.Idx

trait Metadata {
  def shape: Shape[Dimension[Idx]]
  val dtype: DataType
  def compressor: Compressor = Blosc()
  def order: Order = C
  def fill_value: FillValue[T] = Null
  def zarr_format: Format = `2`
  def filters: Opt[Seq[Filter]] = None

  type T = dtype.T
  type Shape[_]
  type Idx
}

object Metadata {

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

  def apply(dir: Path)(implicit idx: Idx): Exception | S[Seq, idx.T] =
    dir ? basename flatMap {
      path ⇒
        decode[S[Seq, idx.T]](path.read)
    }

  implicit def decoder[Idx](
    implicit
    idx: Idx.T[Idx],
    decoder: Decoder[Idx]
  ):
    Decoder[
      S[
        Seq,
        Idx
      ]
    ] =
    new Decoder[S[Seq, Idx]] {
      def apply(c: HCursor): Result[S[Seq, Idx]] = {
        for {
                _shape ← c.downField(      "shape").as[Seq[Idx]]
               _chunks ← c.downField(     "chunks").as[Seq[Idx]]
                _dtype ← c.downField(      "dtype").as[DataType]
           _compressor ← c.downField( "compressor").as[Compressor]
                _order ← c.downField(      "order").as[Order]
          _zarr_format ← c.downField("zarr_format").as[Format]
        } yield
          if (_shape.size == _chunks.size)
            new zarr.Metadata[_dtype.T, Seq, Idx](
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
              _zarr_format
            )
            : S[Seq, Idx]
          else
            throw new IllegalStateException(
              s"Shape and chunks arrays have unequal sizes (${_shape.size} vs ${_chunks.size}): ${_shape.mkString(",")} ${_chunks.mkString(",")}"
            )
      }
    }

  // TODO: are all these basenames necessary?
  implicit val _basename = Basename[Metadata](basename)
  implicit def _basenameShape[S, Idx] = Basename[Metadata.S[S, Idx]](basename)
  implicit def _basenameT    [T, Idx] = Basename[Metadata.T[T, Idx]](basename)
}
