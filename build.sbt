spark
spark.version := "2.2.1"

dep(
  "org.lasersonlab.thredds" ^ "cdm" ^ "5.0.0" snapshot,
  "com.novocode" ^ "junit-interface" ^ "0.11" tests,
  "com.google.cloud" ^ "google-cloud-nio" ^ "0.54.0-alpha",

  hammerlab.bytes % "1.2.0",
  hammerlab.channel % "1.5.0",
  hammerlab.cli.base % "1.0.1" snapshot,
  hammerlab.io % "5.1.1" snapshot,
  hammerlab.lib("types") % "1.3.0" snapshot,
  paths % "1.5.0",

  spark.mllib,
  spark.sql,

  "org.lasersonlab" ^ "s3fs" ^ "2.2.3"
)
