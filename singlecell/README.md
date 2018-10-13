# singlecell

A small, experimental library for reading HDF5 files in parallel from the Cloud using Java and Spark. 

It uses [NetCDF-Java](https://www.unidata.ucar.edu/software/thredds/current/netcdf-java/documentation.htm), modified to
handle dimensions that don't fit into a Java `int` (i.e. greater than 2,147,483,647).
(See [here](https://github.com/tomwhite/thredds/tree/long_dim) for the patched version.)

The process of reading HDF5 files involves a lot of seeks, an access pattern that is not conducive to good performance
with remote stores or high throughput processing. To mitigate this, parts of the file are cached using a
[CachingChannel](https://github.com/hammerlab/io-utils/tree/master/channel).

## Building

```bash
mvn install
```

## Conversion to Parquet

One common need is to convert a HDF5 file to a format that is more readily consumed by Spark, such as Parquet.

See [10x_hdf5_to_parquet.scala](10x_hdf5_to_parquet.scala) for a demonstration of how to convert HDF5 to Parquet using
Spark. Note that the 10x data uses a sparse encoding, which is described
[here](https://support.10xgenomics.com/single-cell-gene-expression/software/pipelines/latest/advanced/h5_matrices).
The data used in this example can be downloaded for free from
[this page](https://support.10xgenomics.com/single-cell-gene-expression/datasets/1.3.0/1M_neurons).

### Performance

To convert a 3.9G 10x file to Parquet takes 11 min, running locally (and reading the file from the local filesystem)
with 2 Spark cores, and 320 tasks.

Running on Google Dataproc with 5 nodes (20 cores) took 4.5 min. So around 2.5 times faster, but with 10 times the
resources (i.e. 4 times as inefficient as local files).

The takeaway is that you can speed up HDF5 processing of very large files by throwing resources at the problem, at the
cost of some inefficiency.
