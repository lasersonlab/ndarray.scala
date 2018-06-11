addSparkDeps
sparkVersion := "2.2.1"

dep(
  "org.lasersonlab.thredds" ^ "cdm" ^ "5.0.0" snapshot,
  "com.novocode" ^ "junit-interface" ^ "0.11" tests,
  "com.google.cloud" ^ "google-cloud-nio" ^ "0.49.0-alpha",

  hammerlab.channel % "1.5.0",

  mllib
)

deps += "org.apache.spark" ^^ "spark-sql" ^ sparkVersion.value
