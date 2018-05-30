package com.tom_e_white.hdf5_java_cloud;

import org.junit.Assert;
import org.junit.Test;
import ucar.ma2.Array;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;
import ucar.unidata.io.RandomAccessFile;

import java.net.URI;
import java.util.List;

public class NioReadOnlyRandomAccessFileTest {
  @Test
  public void test() throws Exception {
    URI uri = Thread.currentThread().getContextClassLoader().getResource("chunked.hdf5").toURI();
    RandomAccessFile raf = new NioReadOnlyRandomAccessFile(uri);
    NetcdfFile ncfile = NetcdfFile.open(raf, uri.toString(), null, null);

    List<Variable> variables = ncfile.getVariables();
    Assert.assertEquals(1, variables.size());
    Variable dataset1 = variables.get(0);
    Assert.assertEquals("dataset1", dataset1.getFullName());
    Assert.assertArrayEquals(new int[]{21, 16}, dataset1.getShape());

    Array values = dataset1.read();
    int[][] valuesArr2d = (int[][]) values.copyToNDJavaArray();

    Assert.assertEquals(21, valuesArr2d.length);
    Assert.assertArrayEquals(new int[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }, valuesArr2d[0]);
    Assert.assertArrayEquals(new int[] { 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 }, valuesArr2d[1]);
  }
}
