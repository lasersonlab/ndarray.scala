package org.lasersonlab.convert

import org.lasersonlab.zarr.{ Dimension, FillValue }
import org.lasersonlab.zarr.dtype.DataType
import ucar.ma2.IndexIterator

/**
 * Wrapper for some properties, and a dependent type, that are being converted from HDF5
 */
abstract class Type {
  type T
  def shape: List[Dimension[Int]]
  def sectionShape: Array[Int]
  def datatype: DataType.Aux[T]
  def fill_value: FillValue[T]
  def next(it: IndexIterator): T
  implicit def encoder: FillValue.Encoder[T]
}
object Type {
  def make[_T](
    _shape: List[Dimension[Int]],
    _sectionShape: Array[Int],
    _datatype: DataType.Aux[_T],
    _fill_value: FillValue[_T],
    _next: IndexIterator ⇒ _T
  )(
    implicit
    _encoder: FillValue.Encoder[_T]
  ): Type =
    new Type {
      type T = _T
      val shape = _shape
      val sectionShape = _sectionShape
      val datatype = _datatype
      val fill_value = _fill_value
      val encoder = _encoder
      @inline def next(it: IndexIterator): T = _next(it)
    }
}

