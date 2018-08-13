package org.lasersonlab.zarr

import cats.Traverse
import hammerlab.path._
import hammerlab.shapeless.tlist._
import org.lasersonlab.ndarray
import org.lasersonlab.ndarray.{ Arithmetic, Sum }
import org.lasersonlab.ndarray.Bytes.Bytes
import org.lasersonlab.zarr.ByteOrder.LittleEndian

class ArrayTest
  extends hammerlab.Suite {
  val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/matrix")
  test("2-D floats") {
    type T = Float
    type Shape = Int :: Int :: TNil
    type Arr[T] =
      //ndarray.Array.Aux[
      Bytes[
        T,
        Shape
      ]

    implicit val float = DataType.float(LittleEndian)
    implicit val indices: Indices[Shape, Arr] = ??? //Indices.cons[Int :: TNil, Arr]
    implicit val traverse: Traverse[Arr] = ???
    implicit val arithmetic: Arithmetic[Shape, Int] = ???
    implicit val sum: Sum.Aux[Shape, Int] = ???

    val arr: Array[T, Shape, Arr] =
      Array[
        Float,
        Shape,
        Arr
      ](
        path
      )(
        d = !!.apply,
        ti = indices,//!!.apply,
        traverse = !!.apply,
        ai = !!.apply,
        scanRight = !!.apply,
        sum = !!.apply,
        dt = !!.apply,
        dect = !!.apply,
        arith = !!.apply,
        key = !!.apply,
        shDec = !!.apply
      )
      .right
      .get
  }
}
