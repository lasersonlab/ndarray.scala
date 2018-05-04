package com.tom_e_white.hdf5_experiments;

import ucar.ma2.Array;
import ucar.ma2.Index;
import ucar.ma2.InvalidRangeException;
import ucar.ma2.Range;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;
import ucar.unidata.io.RandomAccessFile;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Read {
    public static void main(String[] args) throws IOException, InvalidRangeException {
        //NetcdfFile ncfile = NetcdfFile.open("/Users/tom/workspace/hdf5-experiments/files/chunked.hdf5");

        String location = "file:/Users/tom/workspace/hdf5-experiments/files/chunked.hdf5";
        Path path = Paths.get(URI.create(location));
        RandomAccessFile raf = new NioReadOnlyRandomAccessFile(path);
        NetcdfFile ncfile = NetcdfFile.open(raf, location, null, null);
        System.out.println(ncfile);

        List<Variable> variables = ncfile.getVariables();
        for (Variable v : variables) {
            System.out.println(v);
            String fullName = v.getFullName();
            System.out.println(fullName);
            int[] shape = v.getShape();
            System.out.println(Arrays.toString(shape));

            Array array = v.read();
            int[][] o = (int[][]) array.copyToNDJavaArray();
            dump(o);

            // iterate through rows
            for (int r = 0; r < 21; r++) {
                List<Range> ranges = new ArrayList<>();
                ranges.add(new Range(r, r)); // it would be possible to grab chunks of rows here too
                ranges.add(new Range(0, 15));
                Array array2 = v.read(ranges);
                System.out.println(array2);
            }
        }

    }

    private static void dump(int[][] o) {
        for (int i = 0; i < o.length; i++) {
            for (int j = 0; j < o[i].length; j++) {
                System.out.printf("%d, ", o[i][j]);
            }
            System.out.println();
        }
    }
}
