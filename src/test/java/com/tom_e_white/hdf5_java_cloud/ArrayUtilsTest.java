package com.tom_e_white.hdf5_java_cloud;

import org.junit.Assert;
import org.junit.Test;
import ucar.ma2.Array;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import java.util.List;

public class ArrayUtilsTest {

  @Test
  public void test() throws Exception {
    NetcdfFile ncfile = NetcdfFile.open("src/test/resources/chunked.hdf5");

    List<Variable> variables = ncfile.getVariables();
    Assert.assertEquals(1, variables.size());
    Variable dataset1 = variables.get(0);
    Assert.assertEquals("dataset1", dataset1.getFullName());
    Assert.assertArrayEquals(new int[] { 21, 16 }, dataset1.getShape());

    Array values = dataset1.read();
    int[][] valuesArr2d = (int[][]) values.copyToNDJavaArray();

    Assert.assertEquals(21, valuesArr2d.length);
    Assert.assertArrayEquals(new int[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }, valuesArr2d[0]);
    Assert.assertArrayEquals(new int[] { 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 }, valuesArr2d[1]);

    Array row = ArrayUtils.index(dataset1, 1);
    int[][] rowArr2d = (int[][]) row.copyToNDJavaArray();
    Assert.assertEquals(1, rowArr2d.length);
    Assert.assertArrayEquals(new int[] { 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 }, rowArr2d[0]);

    Array rows = ArrayUtils.index(dataset1, 0, 2);
    int[][] rowsArr2d = (int[][]) rows.copyToNDJavaArray();
    Assert.assertEquals(2, rowsArr2d.length);
    Assert.assertArrayEquals(new int[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }, rowsArr2d[0]);
    Assert.assertArrayEquals(new int[] { 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 }, rowsArr2d[1]);
  }
}
