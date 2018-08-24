package org.lasersonlab.zarr

import cats.implicits._
import hammerlab.path._
import hammerlab.shapeless.tlist.{ Map ⇒ _, _ }
import org.lasersonlab.anndata.loom.{ Obs, Var }
import org.lasersonlab.ndarray.Ints._
import org.lasersonlab.zarr.Compressor.Blosc
import org.lasersonlab.zarr.Compressor.Blosc.CName
import org.lasersonlab.zarr.Format.`2`
import org.lasersonlab.zarr.Order.C
import org.lasersonlab.zarr.dtype.ByteOrder.LittleEndian
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.dtype.DataType._
import shapeless.nat._

class ArrayTest
  extends Suite {

  test("2-D floats") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/matrix")

    val arr = Array[Float, _2](path).get

    val metadata = arr.metadata

    metadata should be(
      Metadata(
        shape = 27998 :: 5425 :: TNil,
        chunks = 3092 :: 5425 :: TNil,
        dtype = float(LittleEndian),
        compressor =
          Blosc(
            cname = CName.lz4,
            clevel = 5,
            shuffle = 1,
            blocksize = 0
          ),
        order = C,
        fill_value = 0.0f,
        zarr_format = `2`,
        filters = None
      )
    )

    ==(arr.attrs, None)

    val rows = arr.chunks.rows
    arr.chunks.size should be(10)
    rows.length should be(10)
    rows.foreach(_.size should be(1))
    val chunks = rows.map(_.head)

    val unchecked = scala.Array.fill(1 << 26)(0.toByte)

    val expected =
      Seq(
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 0 :: 0 :: TNil,     0 :: 0:: TNil,  3092 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 1 :: 0 :: TNil,  3092 :: 0:: TNil,  6184 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 2 :: 0 :: TNil,  6184 :: 0:: TNil,  9276 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 3 :: 0 :: TNil,  9276 :: 0:: TNil, 12368 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 4 :: 0 :: TNil, 12368 :: 0:: TNil, 15460 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 5 :: 0 :: TNil, 15460 :: 0:: TNil, 18552 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 6 :: 0 :: TNil, 18552 :: 0:: TNil, 21644 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 7 :: 0 :: TNil, 21644 :: 0:: TNil, 24736 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked, 3092 :: 5425 :: TNil, 8 :: 0 :: TNil, 24736 :: 0:: TNil, 27828 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil),
        Chunk[Float, Ints2](unchecked,  170 :: 5425 :: TNil, 9 :: 0 :: TNil, 27828 :: 0:: TNil, 27998 :: 5425 :: TNil, 16774100, 5425 :: 1 :: TNil)
      )

    chunks
      .zip(expected)
      .foreach {
        case (actual: Chunk[Float, Ints2], expected) ⇒
          ==(actual.start, expected.start)
          ==(actual.end, expected.end)
          ==(actual.idx, expected.idx)
          ==(actual.shape, expected.shape)
      }

    val chunkNonzeroCounts =
      chunks
        .map {
          _.foldLeft(0) {
            (sum, n) ⇒
              if (n > 0)
                sum + 1
              else
                sum
          }
        }

    chunkNonzeroCounts should be(
      Vector(
        287412,
        444234,
        17227,
        58283,
        876917,
        796162,
        582618,
        505650,
        615567,
        36142
      )
    )
  }

  test("1-D longs") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/row_attrs/_Valid")

    val arr = Array[Long, _1](path).get

    import arr.{ == ⇒ _, _ }

    metadata should be(
      Metadata(
         shape = 27998 :: TNil,
        chunks = 27998 :: TNil,
        dtype = long(LittleEndian),
        compressor =
          Blosc(
            cname = CName.lz4,
            clevel = 5,
            shuffle = 1,
            blocksize = 0
          ),
        order = C,
        fill_value = 0L,
        zarr_format = `2`,
        filters = None
      )
    )

    ==(attrs, None)
    chunks.size should be(1)
    val chunk = chunks(0)
    val bytes = chunk.bytes
    bytes.length should be(223984)

    chunk.size should be(27998)

    val nonzeros =
      chunk.foldLeft(0) {
        (sum, n) ⇒
          if (n > 0)
            sum + 1
          else
            sum
      }

    nonzeros should be(11717)
  }

  test("1-D strings") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.loom.64m.zarr/col_attrs/Sex")

    val arr = Array[String, _1](path).get

    import arr.{ == ⇒ _, _ }

    metadata should be(
      Metadata(
         shape = 5425 :: TNil,
        chunks = 5425 :: TNil,
        dtype = string(5),
        compressor =
          Blosc(
            cname = CName.lz4,
            clevel = 5,
            shuffle = 1,
            blocksize = 0
          ),
        order = C,
        fill_value = "",
        zarr_format = `2`,
        filters = None
      )
    )

    ==(attrs, None)

    chunks.size should be(1)

    val chunk = chunks(0)
    chunk.size should be(5425)

    val bytes = chunk.bytes
    bytes.length should be(27125)

    val elems = chunk.toList
    elems.size should be(5425)
    elems.take(10) should be(
      List(
        "1M",
        "F",
        "1F",
        "F",
        "1M 1F",
        "1M 1F",
        "F",
        "1M 1F",
        "F",
        "M"
      )
    )
  }

  test("1-D typed structs") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.ad.32m.zarr/var")

    import shapeless._

    val arr = Array[Var, _1](path).get
    val Array(metadata, attrs, chunks) = arr

    implicit val stringDataType = string(18)
    val dtype = !![DataType.Aux[Var]]

    metadata should be(
      Metadata(
         shape = 27998 :: TNil,
        chunks = 27998 :: TNil,
        dtype = dtype,
        compressor =
          Blosc(
            cname = CName.lz4,
            clevel = 5,
            shuffle = 1,
            blocksize = 0
          ),
        order = C,
        fill_value = Var.empty,
        zarr_format = `2`,
        filters = None
      )
    )

    ==(attrs, None)

    chunks.size should be(1)
    val chunk = chunks(0)
    val bytes = chunk.bytes
    bytes.length should be(1903864)

    chunk.size should be(27998)
    val elems = chunk.toList
    elems.size should be(27998)
    ===(
      elems.take(10),
      Seq(
        Var(0, "ENSMUSG00000022528", gene = 14509, logCV = 0.4815124, logMean =   0.2018570, selected = 1, total = 1616.0, valid = 1),
        Var(1, "ENSMUSG00000058427", gene =  5558, logCV = 3.3051310, logMean = - 5.9969227, selected = 1, total =   22.0, valid = 1),
        Var(2, "ENSMUSG00000015312", gene =  7952, logCV = 0.6754399, logMean = - 0.1837246, selected = 1, total = 1237.0, valid = 1),
        Var(3, "ENSMUSG00000024401", gene = 25608, logCV = 0.0      , logMean =   0.0      , selected = 1, total =    0.0, valid = 1),
        Var(4, "ENSMUSG00000015396", gene =  4382, logCV = 2.4097327, logMean = - 4.5983734, selected = 1, total =   58.0, valid = 1),
        Var(5, "ENSMUSG00000000982", gene =  4230, logCV = 4.8032539, logMean = - 8.8713918, selected = 1, total =    3.0, valid = 1),
        Var(6, "ENSMUSG00000018930", gene =  4231, logCV = 5.2276633, logMean = -10.4563544, selected = 1, total =    1.0, valid = 1),
        Var(7, "ENSMUSG00000038418", gene =  6673, logCV = 0.2999214, logMean =   2.0053807, selected = 1, total = 5641.0, valid = 1),
        Var(8, "ENSMUSG00000042622", gene = 16562, logCV = 1.1246109, logMean = - 1.2789349, selected = 1, total =  579.0, valid = 1),
        Var(9, "ENSMUSG00000053560", gene = 14957, logCV = 0.4013466, logMean =   2.0053807, selected = 1, total = 5641.0, valid = 1)
      )
    )
  }

  test("1-D untyped structs") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.ad.32m.zarr/obs")

    //import shapeless._

    val arr = Array[Obs, _1](path).get

    arr.metadata should be(
      Metadata(
         shape = 5425 :: TNil,
        chunks = 5425 :: TNil,
        dtype =
          DataType.struct(
            List[(String, DataType)](
              ("index", long),
              ("Age", byte),
              ("AnalysisPool", byte),
              ("AnalysisProject", byte),
              ("Bucket", byte),
              ("CellConc", byte),
              ("CellID", string(24)),
              ("Cell_Conc", byte),
              ("ChipID", byte),
              ("Class", byte),
              ("ClassProbability_Astrocyte", double),
              ("ClassProbability_Astrocyte,Immune", double),
              ("ClassProbability_Astrocyte,Neurons", double),
              ("ClassProbability_Astrocyte,Oligos", double),
              ("ClassProbability_Astrocyte,Vascular", double),
              ("ClassProbability_Bergmann-glia", double),
              ("ClassProbability_Blood", double),
              ("ClassProbability_Blood,Vascular", double),
              ("ClassProbability_Enteric-glia", double),
              ("ClassProbability_Enteric-glia,Cycling", double),
              ("ClassProbability_Ependymal", double),
              ("ClassProbability_Ex-Neurons", double),
              ("ClassProbability_Ex-Vascular", double),
              ("ClassProbability_Immune", double),
              ("ClassProbability_Immune,Neurons", double),
              ("ClassProbability_Immune,Oligos", double),
              ("ClassProbability_Neurons", double),
              ("ClassProbability_Neurons,Cycling", double),
              ("ClassProbability_Neurons,Oligos", double),
              ("ClassProbability_Neurons,Satellite-glia", double),
              ("ClassProbability_Neurons,Vascular", double),
              ("ClassProbability_OEC", double),
              ("ClassProbability_Oligos", double),
              ("ClassProbability_Oligos,Cycling", double),
              ("ClassProbability_Oligos,Vascular", double),
              ("ClassProbability_Satellite-glia", double),
              ("ClassProbability_Satellite-glia,Cycling", double),
              ("ClassProbability_Satellite-glia,Schwann", double),
              ("ClassProbability_Schwann", double),
              ("ClassProbability_Ttr", double),
              ("ClassProbability_Vascular", double),
              ("ClusterName", byte),
              ("Clusters", long),
              ("Comment", byte),
              ("Comments", byte),
              ("DateCaptured", byte),
              ("Date_Captured", byte),
              ("Description", byte),
              ("Developmental_compartment", byte),
              ("DonorID", byte),
              ("Estimated Number of Cells", byte),
              ("Flowcell", byte),
              ("Fraction Reads in Cells", byte),
              ("Label", byte),
              ("LeafOrder", long),
              ("Location_based_on", byte),
              ("Mean Reads per Cell", byte),
              ("Median Genes per Cell", byte),
              ("Median UMI Counts per Cell", byte),
              ("MitoRiboRatio", float),
              ("NGI_PlateWell", byte),
              ("Neurotransmitter", byte),
              ("NumPooledAnimals", byte),
              ("Num_Pooled_Animals", byte),
              ("Number of Reads", byte),
              ("OriginalClusters", long),
              ("Outliers", long),
              ("PCRCycles", byte),
              ("PCR_Cycles", byte),
              ("PassedQC", byte),
              ("PlugDate", byte),
              ("Plug_Date", byte),
              ("Probable_location", byte),
              ("Project", byte),
              ("Q30 Bases in Barcode", byte),
              ("Q30 Bases in RNA Read", byte),
              ("Q30 Bases in Sample Index", byte),
              ("Q30 Bases in UMI", byte),
              ("Reads Mapped Confidently to Exonic Regions", byte),
              ("Reads Mapped Confidently to Intergenic Regions", byte),
              ("Reads Mapped Confidently to Intronic Regions", byte),
              ("Reads Mapped Confidently to Transcriptome", byte),
              ("Region", byte),
              ("SampleID", byte),
              ("SampleIndex", byte),
              ("SampleOK", byte),
              ("Sample_Index", byte),
              ("SeqComment", byte),
              ("SeqLibDate", byte),
              ("SeqLibOk", byte),
              ("Seq_Comment", byte),
              ("Seq_Lib_Date", byte),
              ("Seq_Lib_Ok", byte),
              ("Sequencing Saturation", byte),
              ("Serial_Number", byte),
              ("Sex", byte),
              ("Species", byte),
              ("Strain", byte),
              ("Subclass", byte),
              ("TargetNumCells", byte),
              ("Target_Num_Cells", byte),
              ("TaxonomyRank1", byte),
              ("TaxonomyRank2", byte),
              ("TaxonomyRank3", byte),
              ("TaxonomyRank4", byte),
              ("TaxonomySymbol", byte),
              ("Taxonomy_group", byte),
              ("TimepointPool", byte),
              ("Tissue", byte),
              ("Total Genes Detected", byte),
              ("Transcriptome", byte),
              ("Valid Barcodes", byte),
              ("_KMeans_10", double),
              ("_LogCV", double),
              ("_LogMean", double),
              ("_NGenes", double),
              ("_PC1", double),
              ("_PC2", double),
              ("_Total", double),
              ("_Valid", long),
              ("_X", double),
              ("_Y", double),
              ("_tSNE1", double),
              ("_tSNE2", double),
              ("cDNAConcNanogramPerMicroliter", byte),
              ("cDNALibOk", byte),
              ("cDNA_Lib_Ok", byte),
              ("ngperul_cDNA", byte)
            )
            .map {
              case       (name, datatype) ⇒
              StructEntry(name, datatype)
            }
          ),
        compressor =
          Blosc(
            cname = CName.lz4,
            clevel = 5,
            shuffle = 1,
            blocksize = 0
          ),
        order = C,
        fill_value =
          untyped.Struct(
            Map(
              "index" → 0L,
              "Age" → 0.toByte,
              "AnalysisPool" → 0.toByte,
              "AnalysisProject" → 0.toByte,
              "Bucket" → 0.toByte,
              "CellConc" → 0.toByte,
              "CellID" → "",
              "Cell_Conc" → 0.toByte,
              "ChipID" → 0.toByte,
              "Class" → 0.toByte,
              "ClassProbability_Astrocyte" → 0.0,
              "ClassProbability_Astrocyte,Immune" → 0.0,
              "ClassProbability_Astrocyte,Neurons" → 0.0,
              "ClassProbability_Astrocyte,Oligos" → 0.0,
              "ClassProbability_Astrocyte,Vascular" → 0.0,
              "ClassProbability_Bergmann-glia" → 0.0,
              "ClassProbability_Blood" → 0.0,
              "ClassProbability_Blood,Vascular" → 0.0,
              "ClassProbability_Enteric-glia" → 0.0,
              "ClassProbability_Enteric-glia,Cycling" → 0.0,
              "ClassProbability_Ependymal" → 0.0,
              "ClassProbability_Ex-Neurons" → 0.0,
              "ClassProbability_Ex-Vascular" → 0.0,
              "ClassProbability_Immune" → 0.0,
              "ClassProbability_Immune,Neurons" → 0.0,
              "ClassProbability_Immune,Oligos" → 0.0,
              "ClassProbability_Neurons" → 0.0,
              "ClassProbability_Neurons,Cycling" → 0.0,
              "ClassProbability_Neurons,Oligos" → 0.0,
              "ClassProbability_Neurons,Satellite-glia" → 0.0,
              "ClassProbability_Neurons,Vascular" → 0.0,
              "ClassProbability_OEC" → 0.0,
              "ClassProbability_Oligos" → 0.0,
              "ClassProbability_Oligos,Cycling" → 0.0,
              "ClassProbability_Oligos,Vascular" → 0.0,
              "ClassProbability_Satellite-glia" → 0.0,
              "ClassProbability_Satellite-glia,Cycling" → 0.0,
              "ClassProbability_Satellite-glia,Schwann" → 0.0,
              "ClassProbability_Schwann" → 0.0,
              "ClassProbability_Ttr" → 0.0,
              "ClassProbability_Vascular" → 0.0,
              "ClusterName" → 0.toByte,
              "Clusters" → 0L,
              "Comment" → 0.toByte,
              "Comments" → 0.toByte,
              "DateCaptured" → 0.toByte,
              "Date_Captured" → 0.toByte,
              "Description" → 0.toByte,
              "Developmental_compartment" → 0.toByte,
              "DonorID" → 0.toByte,
              "Estimated Number of Cells" → 0.toByte,
              "Flowcell" → 0.toByte,
              "Fraction Reads in Cells" → 0.toByte,
              "Label" → 0.toByte,
              "LeafOrder" → 0L,
              "Location_based_on" → 0.toByte,
              "Mean Reads per Cell" → 0.toByte,
              "Median Genes per Cell" → 0.toByte,
              "Median UMI Counts per Cell" → 0.toByte,
              "MitoRiboRatio" → 0.0f,
              "NGI_PlateWell" → 0.toByte,
              "Neurotransmitter" → 0.toByte,
              "NumPooledAnimals" → 0.toByte,
              "Num_Pooled_Animals" → 0.toByte,
              "Number of Reads" → 0.toByte,
              "OriginalClusters" → 0L,
              "Outliers" → 0L,
              "PCRCycles" → 0.toByte,
              "PCR_Cycles" → 0.toByte,
              "PassedQC" → 0.toByte,
              "PlugDate" → 0.toByte,
              "Plug_Date" → 0.toByte,
              "Probable_location" → 0.toByte,
              "Project" → 0.toByte,
              "Q30 Bases in Barcode" → 0.toByte,
              "Q30 Bases in RNA Read" → 0.toByte,
              "Q30 Bases in Sample Index" → 0.toByte,
              "Q30 Bases in UMI" → 0.toByte,
              "Reads Mapped Confidently to Exonic Regions" → 0.toByte,
              "Reads Mapped Confidently to Intergenic Regions" → 0.toByte,
              "Reads Mapped Confidently to Intronic Regions" → 0.toByte,
              "Reads Mapped Confidently to Transcriptome" → 0.toByte,
              "Region" → 0.toByte,
              "SampleID" → 0.toByte,
              "SampleIndex" → 0.toByte,
              "SampleOK" → 0.toByte,
              "Sample_Index" → 0.toByte,
              "SeqComment" → 0.toByte,
              "SeqLibDate" → 0.toByte,
              "SeqLibOk" → 0.toByte,
              "Seq_Comment" → 0.toByte,
              "Seq_Lib_Date" → 0.toByte,
              "Seq_Lib_Ok" → 0.toByte,
              "Sequencing Saturation" → 0.toByte,
              "Serial_Number" → 0.toByte,
              "Sex" → 0.toByte,
              "Species" → 0.toByte,
              "Strain" → 0.toByte,
              "Subclass" → 0.toByte,
              "TargetNumCells" → 0.toByte,
              "Target_Num_Cells" → 0.toByte,
              "TaxonomyRank1" → 0.toByte,
              "TaxonomyRank2" → 0.toByte,
              "TaxonomyRank3" → 0.toByte,
              "TaxonomyRank4" → 0.toByte,
              "TaxonomySymbol" → 0.toByte,
              "Taxonomy_group" → 0.toByte,
              "TimepointPool" → 0.toByte,
              "Tissue" → 0.toByte,
              "Total Genes Detected" → 0.toByte,
              "Transcriptome" → 0.toByte,
              "Valid Barcodes" → 0.toByte,
              "_KMeans_10" → 0.0,
              "_LogCV" → 0.0,
              "_LogMean" → 0.0,
              "_NGenes" → 0.0,
              "_PC1" → 0.0,
              "_PC2" → 0.0,
              "_Total" → 0.0,
              "_Valid" → 0L,
              "_X" → 0.0,
              "_Y" → 0.0,
              "_tSNE1" → 0.0,
              "_tSNE2" → 0.0,
              "cDNAConcNanogramPerMicroliter" → 0.toByte,
              "cDNALibOk" → 0.toByte,
              "cDNA_Lib_Ok" → 0.toByte,
              "ngperul_cDNA" → 0.toByte,
            )
          ),
        zarr_format = `2`,
        filters = None
      )
    )

    ==(arr.attrs, None)
  }
}
