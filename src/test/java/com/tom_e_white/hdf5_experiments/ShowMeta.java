package com.tom_e_white.hdf5_experiments;

import ucar.ma2.Array;
import ucar.ma2.InvalidRangeException;
import ucar.ma2.Range;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ShowMeta {
    public static void main(String[] args) throws IOException, InvalidRangeException {
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "debug");

        NetcdfFile ncfile = NetcdfFile.open(args[0]);
        System.out.println(ncfile);

        List<Variable> variables = ncfile.getVariables();
        for (Variable v : variables) {
            System.out.println(v);
        }
    }
}
