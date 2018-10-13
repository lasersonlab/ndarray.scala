# ndarray.scala
[![Build Status](https://travis-ci.org/lasersonlab/ndarray.scala.svg?branch=master)](https://travis-ci.org/lasersonlab/ndarray.scala)
[![codecov](https://codecov.io/gh/lasersonlab/ndarray.scala/branch/master/graph/badge.svg)](https://codecov.io/gh/lasersonlab/ndarray.scala)

Tools for working with N-dimensional arrays, [HDF5], and [Zarr].

Modules:

- [`zarr`](zarr): pure-Scala [Zarr] library
- [`convert`](convert): CLI for converting [HDF5] files to [Zarr] directly in "the cloud" (AWS, GCP)
- [`ndarray`](ndarray): type-safe N-dimensional array-implementation
- [`cloud`](cloud): utilities for interacting with public clouds
- [`netcdf`](netcdf): Scala implementation of [NetCDF] data-structures
- [`utils`](utils): NetCDF [`RandomAccessFile`] using Java NIO (for public-cloud support) 
- [`xscala`](xscala): shims for supporting scala-version cross-compilation 

[HDF5]: https://portal.hdfgroup.org/display/HDF5/HDF5
[Zarr]: https://zarr.readthedocs.io/en/stable/
[NetCDF]: https://www.unidata.ucar.edu/software/netcdf/

[`RandomAccessFile`]: https://github.com/Unidata/thredds/blob/v5.0.0-beta5/cdm/src/main/java/ucar/unidata/io/RandomAccessFile.java
