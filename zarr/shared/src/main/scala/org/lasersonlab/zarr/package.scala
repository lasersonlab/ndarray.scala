package org.lasersonlab

import org.hammerlab.paths.HasPathOps
import org.lasersonlab.ndarray.Arithmetic
import org.lasersonlab.zarr.io.{ Load, Save }
import org.lasersonlab.zarr.opt.OptCodec
import org.lasersonlab.zarr.tlist.TListCodec

/**
 * Spec / Format questions:
 *
 * Is a .zgroup entry really needed in every "directory" above an array? Doesn't that conflict with treating the paths
 * as ≈opaque blobs and not requiring filesystem-semantics in a backing store?
 *
 * Is "" an allowed fill_value for a fixed-length string datatype, e.g. "|S12"? See question in
 * [[zarr.FillValue.Decoder.string]]'s docs
 *
 * When is `fill_value` used? Are there sparseness facilities baked in to Zarr? Aren't chunks read in always of size
 * ${datatype.size} * $num_records?
 *
 * Does the reference implementation write extremal/boundary chunks as smaller, or compress a them as a buffer the size
 * of full-sized/interior chunks? Seems like the latter… some edges to check here around handling that as well.
 */
package object zarr
  extends Arithmetic.HasOps
     with OptCodec
     with TListCodec
     with HasPathOps
     with hammerlab.either
     with hammerlab.math.utils
     with Load.syntax
     with Save.syntax
