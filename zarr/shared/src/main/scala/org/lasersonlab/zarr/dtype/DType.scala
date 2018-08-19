package org.lasersonlab.zarr.dtype

import hammerlab.shapeless._

sealed abstract class DType(override val toString: String)
object DType {
  case object     int extends DType("i")
  case object    bool extends DType("b")
  case object   float extends DType("f")
  case object  string extends DType("S")

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

  val get = InstanceMap[DType]()
}
