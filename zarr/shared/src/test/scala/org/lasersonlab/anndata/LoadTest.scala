package org.lasersonlab.anndata

import cats.implicits._
import hammerlab.path._
import hammerlab.shapeless.tlist._
import org.lasersonlab.anndata.loom.{ Obs, Var }
import org.lasersonlab.zarr.Suite
import org.lasersonlab.zarr.dtype.DataType
import org.lasersonlab.zarr.dtype.DataType.string

class LoadTest
  extends Suite {
  test("load") {
    val path = Path("/Users/ryan/c/hdf5-experiments/files/L6_Microglia.ad.32m.zarr")

    val ad = path.load[AnnData[Obs, Var]].get

    val AnnData(x, obs, v, uns) = ad

    ==(x.shape,     5425 :: 27998 :: TNil)
    ==(x.chunkShape, 299 :: 27998 :: TNil)
    ==(x.foldLeft(0.0f)(_ + _), 8596396.0f)

    ==(obs.shape,      5425 :: TNil)
    ==(obs.chunkShape, 5425 :: TNil)

    ==(v.shape,      27998 :: TNil)
    ==(v.chunkShape, 27998 :: TNil)

    ==(uns.arrays.size, 79)
    ==(uns.groups.size, 0)
    ==(uns.attrs, None)

    val arrays = uns.arrays
    ==(
      arrays
        .toSeq
        .sortBy(_._1)
        .map {
          case (k, arr) ⇒
            val Seq(shape) = arr.shape
            shape → (arr.metadata.dtype: DataType) → k
        },
      Seq[((Int, DataType), String)](
           19 → string( 8) → "Age_categories",
            1 → string( 1) → "AnalysisPool_categories",
            1 → string( 1) → "AnalysisProject_categories",
            1 → string(41) → "Bucket_categories",
            1 → string( 1) → "CellConc_categories",
            1 → string( 1) → "Cell_Conc_categories",
            1 → string( 1) → "ChipID_categories",
            1 → string( 6) → "Class_categories",
            3 → string( 4) → "ClusterName_categories",
            1 → string( 3) → "Comment_categories",
            1 → string( 1) → "Comments_categories",
            1 → string( 1) → "DateCaptured_categories",
            1 → string( 1) → "Date_Captured_categories",
            2 → string(20) → "Description_categories",
            1 → string( 8) → "Developmental_compartment_categories",
            1 → string( 1) → "DonorID_categories",
            1 → string( 1) → "Estimated Number of Cells_categories",
            1 → string( 1) → "Flowcell_categories",
            1 → string( 1) → "Fraction Reads in Cells_categories",
        27933 → string(14) → "Gene_categories",
            1 → string( 1) → "Label_categories",
            3 → string(19) → "Location_based_on_categories",
            1 → string( 1) → "Mean Reads per Cell_categories",
            1 → string( 1) → "Median Genes per Cell_categories",
            1 → string( 1) → "Median UMI Counts per Cell_categories",
            1 → string( 1) → "NGI_PlateWell_categories",
            1 → string( 1) → "Neurotransmitter_categories",
            1 → string( 1) → "NumPooledAnimals_categories",
            1 → string( 1) → "Num_Pooled_Animals_categories",
            1 → string( 1) → "Number of Reads_categories",
            1 → string( 1) → "PCRCycles_categories",
            1 → string( 1) → "PCR_Cycles_categories",
            1 → string( 1) → "PassedQC_categories",
            1 → string( 1) → "PlugDate_categories",
            1 → string( 1) → "Plug_Date_categories",
            1 → string( 3) → "Probable_location_categories",
            1 → string( 1) → "Project_categories",
            1 → string( 1) → "Q30 Bases in Barcode_categories",
            1 → string( 1) → "Q30 Bases in RNA Read_categories",
            1 → string( 1) → "Q30 Bases in Sample Index_categories",
            1 → string( 1) → "Q30 Bases in UMI_categories",
            1 → string( 1) → "Reads Mapped Confidently to Exonic Regions_categories",
            1 → string( 1) → "Reads Mapped Confidently to Intergenic Regions_categories",
            1 → string( 1) → "Reads Mapped Confidently to Intronic Regions_categories",
            1 → string( 1) → "Reads Mapped Confidently to Transcriptome_categories",
            1 → string( 3) → "Region_categories",
          113 → string( 7) → "SampleID_categories",
            1 → string( 1) → "SampleIndex_categories",
            1 → string( 1) → "SampleOK_categories",
            1 → string( 1) → "Sample_Index_categories",
            1 → string( 1) → "SeqComment_categories",
            1 → string( 1) → "SeqLibDate_categories",
            1 → string( 1) → "SeqLibOk_categories",
            1 → string( 1) → "Seq_Comment_categories",
            1 → string( 1) → "Seq_Lib_Date_categories",
            1 → string( 1) → "Seq_Lib_Ok_categories",
            1 → string( 1) → "Sequencing Saturation_categories",
            1 → string( 1) → "Serial_Number_categories",
           10 → string( 5) → "Sex_categories",
            1 → string( 1) → "Species_categories",
            1 → string( 1) → "Strain_categories",
            1 → string( 6) → "Subclass_categories",
            1 → string( 1) → "TargetNumCells_categories",
            1 → string( 1) → "Target_Num_Cells_categories",
            1 → string(12) → "TaxonomyRank1_categories",
            1 → string(12) → "TaxonomyRank2_categories",
            1 → string(12) → "TaxonomyRank3_categories",
            1 → string( 9) → "TaxonomyRank4_categories",
            1 → string( 4) → "TaxonomySymbol_categories",
            1 → string( 9) → "Taxonomy_group_categories",
            1 → string( 1) → "TimepointPool_categories",
           20 → string(10) → "Tissue_categories",
            1 → string( 1) → "Total Genes Detected_categories",
            1 → string( 1) → "Transcriptome_categories",
            1 → string( 1) → "Valid Barcodes_categories",
            1 → string( 1) → "cDNAConcNanogramPerMicroliter_categories",
            1 → string( 1) → "cDNALibOk_categories",
            1 → string( 1) → "cDNA_Lib_Ok_categories",
            1 → string( 1) → "ngperul_cDNA_categories"
      )
    )
  }
}
