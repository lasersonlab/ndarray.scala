spark
spark.version := "2.2.1"

dep(
  // lasersonlab forks of netCDF, cloud-store dependencies
  "org.lasersonlab.thredds" ^ "cdm" ^ "5.0.0",
  "org.lasersonlab" ^ "google-cloud-nio" ^ "0.55.2-alpha",
  "org.lasersonlab" ^ "s3fs" ^ "2.2.3",

  hammerlab.bytes % "1.2.0",
  hammerlab.channel % "1.5.2",
  hammerlab.cli.base % "1.0.1",
  hammerlab.io % "5.2.0",
  hammerlab.types % "1.3.0",
  paths % "1.5.0",

  spark.mllib,
  spark.sql,

  "com.novocode" ^ "junit-interface" ^ "0.11" tests
)
