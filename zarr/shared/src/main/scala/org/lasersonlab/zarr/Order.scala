package org.lasersonlab.zarr

/**
 * Either “C” or “F”, defining the layout of bytes within each chunk of the array.
 *
 * - “C” means row-major order, i.e., the last dimension varies fastest
 * - “F” means column-major order, i.e., the first dimension varies fastest
 */
sealed trait Order
object Order {
  case object C extends Order
  // column-major order not implemented (yet?)
//  case object F extends Order
}
