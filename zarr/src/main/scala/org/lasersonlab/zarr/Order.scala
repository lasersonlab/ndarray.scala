package org.lasersonlab.zarr

sealed trait Order
object Order {
  case object C extends Order
  case object F extends Order
}
