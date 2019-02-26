import hammerlab.{ math, types }
import org.scalajs.jsenv.nodejs.NodeJSEnv
import scalajs._

val        cdm  = "org.lasersonlab.thredds" ^ "cdm" ^ "5.0.0"
val concurrent  = "org.lasersonlab" ^^     "concurrent" ^ "0.1.0"
val      files  = "org.lasersonlab" ^^ "portable-files" ^ "0.1.0"
val   `gcp-nio` = "org.lasersonlab" ^ "google-cloud-nio" ^ "0.55.2-alpha"

default(
  group("org.lasersonlab"),
  versions(
    hammerlab.          bytes → "1.3.0",
    hammerlab.        channel → "1.5.3",
    hammerlab.       cli.base → "1.0.1",
    hammerlab.     math.utils → "2.4.0",
    hammerlab.          paths → "1.6.0",
    hammerlab.          types → "1.5.0",
    hammerlab.shapeless_utils → "1.5.1",
    hammerlab.             io → "5.2.1",

    diode.react → "1.1.4.131",
    dom → "0.9.6"
  ),
  circe.version := "0.11.1",
  http4s.version := "0.20.0-M1",
  diode.version := "1.1.4",
)

lazy val blosc =
  cross
    .settings(
      dep(math.utils)
    )
lazy val `blosc-x` = blosc.x

lazy val cloud =
  cross
    .in(new File("cloud"))
    .settings(
      subgroup("cloud", "all")
    )
    .dependsOn(aws, gcp)
    .aggregate(aws, gcp)
lazy val `cloud-x` = cloud.x

lazy val aws =
  cross
    .in(new File("cloud") / "aws")
    .settings(
      subgroup("cloud")
    )
    .jvmSettings(
      dep(
        "org.lasersonlab" ^ "s3fs" ^ "2.2.3"
      )
    )
lazy val `aws-x` = aws.x

lazy val gcp =
  cross
    .in(new File("cloud") / "gcp")
    .settings(
      subgroup("cloud"),
      dep(
        cats,
        circe,
        circe.generic,
        files ,
        hammerlab.types,
        shapeless,
        sttp
      ),
      enableMacroParadise
    )
    .jsSettings(
      dep(
        dom
      )
    )
    .jvmSettings(
      dep(
        `gcp-nio`
      )
    )
    .dependsOn(
      `circe-utils`
    )
lazy val `gcp-x` = gcp.x

lazy val `circe-utils` =
  cross
    .in(new File("circe"))
    .settings(
      dep(
        circe,
        circe.generic,
        hammerlab.shapeless_utils,
        shapeless
      ),
    )
lazy val `circe-utils-x` = `circe-utils`.x

lazy val convert =
  project
    .settings(
      dep(
        circe,
        concurrent,

        hammerlab.cli.base,
        hammerlab.io,
        hammerlab.paths,
      ),
      // Test-resources include "hidden" (basenames starting with ".") Zarr-metadata files that we need to include on the
      // test classpath for tests to be able to read them. TOOD: factor out to plugin
      includeHiddenTestResources,
      partialUnification,
      utest
    )
    .dependsOn(
      cloud.jvm,
      netcdf,
      testing.jvm forTests,
      utils,
      zarr.jvm andTest
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
  cloud.jvm,
  utils
)

lazy val singlecell = project.settings(
  spark,
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
lazy val `slist-x` = slist.x

lazy val testing =
  cross
    .settings(
      dep(
        cats,
        concurrent,
        files,
        hammerlab.test.suite compile,
        magnolia,
        utest compile
      ),
      utest
    )
lazy val `testing-x` = testing.x

lazy val utils = project.settings(
  dep(
    hammerlab.channel,

    cdm,
    junit tests
  )
)

// Stubs for a "viewer" webapp (client+server)
lazy val viewerCommon =
  cross
    .settings(
      dep(
        scalatags,
        circe,
        circe.generic,
        circe.parser
      )
    )

lazy val viewerClient =
  project
    .settings(

      // cf. https://github.com/scalacenter/scalajs-bundler/issues/278; TODO: move to JS plugin?
      version in startWebpackDevServer := "3.1.14",

      react,
      dep(
        cats,
        concurrent,

        circe,
        circe.generic,
        circe.parser,

        diode,
        diode.react,

        dom,
        react.extra,

        files,
        hammerlab.types,
        scalajs.time,
        sttp,
      ),
      webpackBundlingMode := BundlingMode.LibraryAndApplication(),
      enableMacroParadise,
      kindProjector,
      partialUnification,
      scalaJSUseMainModuleInitializer := true,

      utest,

      npmDependencies in Compile ++=
        Seq(
          "pako"        → "1.0.7",
          "react-proxy" → "1.1.8"
        ),
    )
    .enablePlugins(JS, ScalaJSBundlerPlugin)
    .dependsOn(
      gcp.js,
      viewerCommon.js,

      testing.js forTests
    )

lazy val viewerServer =
  project
    .settings(
      dep(
        http4s.`blaze-server`,
        http4s. circe,
        http4s. dsl,
      ),
      // Allows to read the JS generated by client
      resources in Compile ++= {
        (webpack in (viewerClient, Compile, fastOptJS)).value.map(_.data)
      },
      // rebuild JS on reStart
      reStart := (reStart dependsOn (webpack in (viewerClient, Compile, fastOptJS))).evaluated,
      // reStart if a client scala.js file changes
      watchSources ++= (watchSources in viewerClient).value,
      fork := false
    )
    .dependsOn(
      viewerCommon.jvm
    )

lazy val  xscala    = cross.settings()
lazy val `xscala-x` = xscala.x

lazy val zarr =
  cross
    .settings(
      dep(
        circe,
        circe.generic,
        circe.parser,

        concurrent,
        files,

        hammerlab.bytes,
        hammerlab.io,
        hammerlab.math.utils,
        hammerlab.shapeless_utils,
        hammerlab.types,

        kittens,
        magnolia,
        sourcecode,
      ),
      utest,
      kindProjector,
      partialUnification,
      includeHiddenTestResources,
    )
    .jvmSettings(
      dep(
        "org.lasersonlab" ^ "jblosc" ^ "1.0.1"
      )
    )
    .jsSettings(
      jsDependencies += "org.webjars.npm" % "pako" % "1.0.7" / "pako.js",
      jsEnv := new NodeJSEnv(NodeJSEnv.Config().withArgs("--max-old-space-size=4096" :: Nil))  // TODO: factor out
    )
    .dependsOn(
      `circe-utils`,
       ndarray,
         slist,
       testing forTests,
        xscala
    )
lazy val `zarr-x` = zarr.x

lazy val all =
  root(
          `blosc-x` ,
    `circe-utils-x` ,
          `cloud-x` ,
         convert    ,
        `ndarray-x` ,
          netcdf    ,
      singlecell    ,
          `slist-x` ,
        `testing-x` ,
           utils    ,
         `xscala-x` ,
           `zarr-x` ,
  )
