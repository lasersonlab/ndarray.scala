
default(
  group("org.lasersonlab"),
  versions(
    hammerlab.          bytes → "1.2.0",
    hammerlab.        channel → "1.5.3",
    hammerlab.       cli.base → "1.0.1",
    hammerlab.     math.utils → "2.3.0",
    hammerlab.          paths → "1.6.0",
    hammerlab.          types → "1.4.0",
    hammerlab.shapeless_utils → "1.5.1",
    hammerlab.             io → "5.2.1"
  )
)

lazy val cloud =
  project
    .in(new File("cloud"))
    .settings(
      subgroup("cloud", "all")
    )
    .dependsOn(aws, gcp)
    .aggregate(aws, gcp)

lazy val aws =
  project
    .in(new File("cloud") / "aws")
    .settings(
      subgroup("cloud"),
      dep(
        "org.lasersonlab" ^ "s3fs" ^ "2.2.3"
      )
    )

lazy val gcp =
  project
    .in(new File("cloud") / "gcp")
    .settings(
      subgroup("cloud"),
      dep(
        "org.lasersonlab" ^ "google-cloud-nio" ^ "0.55.2-alpha",
        hammerlab.types
      )
    )

lazy val convert =
  project
    .settings(
      dep(
        hammerlab.cli.base,
        hammerlab.io,
        hammerlab.paths,
      ),
      // Test-resources include "hidden" (basenames starting with ".") Zarr-metadata files that we need to include on the
      // test classpath for tests to be able to read them
      excludeFilter in sbt.Test := NothingFilter,
      partialUnification
    )
    .dependsOn(
      cloud,
      netcdf,
      zarr andTest
    )

lazy val ndarray =
  cross
    .settings(
      dep(
        cats,
        hammerlab.shapeless_utils,
        hammerlab.types,
        shapeless
      ),
      kindProjector.settings,
      partialUnification
    )
    .dependsOn(
      slist
    )
lazy val `ndarray-x` = ndarray.x

lazy val netcdf = project.settings(
  dep(
    hammerlab.bytes,
    hammerlab.cli.base,
    hammerlab.io,
    hammerlab.paths,
    hammerlab.types,
  )
).dependsOn(
  cloud,
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

lazy val slist = cross.settings(
  dep(
    cats,
    hammerlab.types
  )
)

lazy val utils = project.settings(
  crossPaths := false,
  dep(
    hammerlab.channel,

    "org.lasersonlab.thredds" ^ "cdm" ^ "5.0.0",
    "com.novocode" ^ "junit-interface" ^ "0.11" tests
  )
)

// Stubs for a "viewer" webapp (client+server)
lazy val viewerCommon = cross.settings()
lazy val viewerServer = project.settings().dependsOn(viewerCommon.jvm)
lazy val viewerClient = project.settings().dependsOn(viewerCommon.js )

lazy val  xscala    = cross.settings()
lazy val `xscala-x` = xscala.x

// TODO: make x-platform blosc, cross-build zarr
lazy val zarr =
  project
    .settings(
      dep(
        circe,
        circe.generic,
        circe.parser,
        hammerlab.io,
        hammerlab.math.utils,
        hammerlab.paths,
        hammerlab.shapeless_utils,
        hammerlab.types,

        sourcecode,

        "org.typelevel" ^^ "kittens" ^ "1.1.0",
        "org.lasersonlab" ^ "jblosc" ^ "1.0.1",
        "com.propensive" ^^ "magnolia" ^ "0.10.0"
      ),
      kindProjector,
      partialUnification,
      excludeFilter in sbt.Test := NothingFilter
    )
    .dependsOn(
      ndarray.jvm,
        slist.jvm,
       xscala.jvm
    )

lazy val all = root(
   cloud,
   convert,
  `ndarray-x`,
   netcdf,
   singlecell,
   slist,
   utils,
  `xscala-x`,
   zarr
)
