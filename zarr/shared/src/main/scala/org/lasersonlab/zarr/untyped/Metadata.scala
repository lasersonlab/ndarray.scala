package org.lasersonlab.zarr.untyped

import hammerlab.option._
import hammerlab.path._
import io.circe._
import Decoder.Result
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

trait Metadata {
  def shape: Shape
  def chunks: Shape
  val dtype: DataType
  def compressor: Compressor = Blosc()
  def order: Order = C
  def fill_value: FillValue[T] = Null
  def zarr_format: Format = `2`
  def filters: Opt[Seq[Filter]] = None

  type T = dtype.T
  type Shape
}

object Metadata {
  type T[_T] = Metadata { type T = _T }
  type S[_S] = Metadata { type Shape = _S }
  type Aux[_T, _S] =
    Metadata {
      type T = _T
      type Shape = _S
    }

  def apply(dir: Path): Exception | S[Seq[Int]] =
    dir ? basename flatMap {
      path ⇒
        decode[S[Seq[Int]]](path.read)
    }

  implicit val decoder: Decoder[S[Seq[Int]]] =
    new Decoder[S[Seq[Int]]] {
      def apply(c: HCursor): Result[S[Seq[Int]]] = {
        for {
                _shape ← c.downField(      "shape").as[Seq[Int]]
               _chunks ← c.downField(     "chunks").as[Seq[Int]]
                _dtype ← c.downField(      "dtype").as[DataType]
           _compressor ← c.downField( "compressor").as[Compressor]
                _order ← c.downField(      "order").as[Order]
          _zarr_format ← c.downField("zarr_format").as[Format]
        } yield
          new zarr.Metadata[_dtype.T, Seq[Int]](
            _shape,
            _chunks,
            _dtype,
            _compressor,
            _order,
            FillValue.Null,
            _zarr_format
          ): S[Seq[Int]]
      }
    }

  implicit val _basename = Basename[Metadata](basename)
  implicit def _basenameShape[S] = Basename[Metadata.S[S]](basename)
  implicit def _basenameT    [T] = Basename[Metadata.T[T]](basename)
}
