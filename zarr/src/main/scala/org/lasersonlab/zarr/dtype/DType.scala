package org.lasersonlab.zarr.dtype

import hammerlab.shapeless._

sealed abstract class DType(override val toString: String)
object DType {
  val    int = org.lasersonlab.zarr.dtype.   int
  val   bool = org.lasersonlab.zarr.dtype.  bool
  val  float = org.lasersonlab.zarr.dtype. float
  val string = org.lasersonlab.zarr.dtype.string

  type    int =    int.type
  type   bool =   bool.type
  type  float =  float.type
  type string = string.type

  // not implemented (yet?):
  // - V: void *
  // - u: unsigned integer types (little awkward on JVM; maybe doable)
  // - U: Py_UNICODE
  // - m: timedelta
  // - M: datetime
  // - c: "complex floating point"

  val map = InstanceMap[DType]()
}

// Defined out here to avoid knownDirectSubclasses issue in 2.11
case object     int extends DType("i")
case object    bool extends DType("b")
case object   float extends DType("f")
case object  string extends DType("S")
