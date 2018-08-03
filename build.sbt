
default(
  group("org.lasersonlab"),
  versions(
    hammerlab.types → "1.3.0",
    hammerlab.io → "5.2.0"
  )
)

lazy val ndarray = project.in(new File("ndarray/shared")).settings(
  dep(
    hammerlab.shapeless_utils % "1.5.0" snapshot,
    shapeless
  ),
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.7" cross CrossVersion.binary)
)
//lazy val `ndarray.jvm` = ndarray.jvm
//lazy val `ndarray.js`  = ndarray.js
//lazy val `ndarray-x`   = parent(`ndarray.jvm`, `ndarray.js`)

lazy val netcdf = project.settings(
  dep(
    `google-cloud-nio`,
    s3fs,

    hammerlab.bytes % "1.2.0",
    hammerlab.cli.base % "1.0.1",
    hammerlab.io,
    hammerlab.types,
    paths % "1.5.0"
  )
).dependsOn(
  utils
)


lazy val singlecell = project.settings(
  spark,
  spark.version := "2.2.1",
  dep(
    spark.mllib,
    spark.sql
  )
).dependsOn(
  utils
)

lazy val utils = project.settings(
  dep(
    thredds,
    hammerlab.channel % "1.5.2",

    junit
  )
)

lazy val zarr = project.settings(
  dep(
    hammerlab.types
  )
)

lazy val `hdf5-java-cloud` = root(
  ndarray,
  netcdf,
  singlecell,
  utils,
  zarr
)

val `google-cloud-nio` = "org.lasersonlab" ^ "google-cloud-nio" ^ "0.55.2-alpha"
val s3fs = "org.lasersonlab" ^ "s3fs" ^ "2.2.3"

val thredds = "org.lasersonlab.thredds" ^ "cdm" ^ "5.0.0"

val junit = "com.novocode" ^ "junit-interface" ^ "0.11" tests
