import sbt._

object settings {

  val cdm = "edu.ucar" % "cdm" % "5.0.0-SNAPSHOT"
  val channel = "org.hammerlab" %% "channel" % "1.4.0"
  val junit = "com.novocode" % "junit-interface" % "0.11" % "test"
  val `google-cloud-nio` = "com.google.cloud" % "google-cloud-nio" % "0.49.0-alpha"

  object spark {
    val version = "2.2.1"
    def lib(name: String) = "org.apache.spark" %% s"spark-$name" % version
    val core = lib("core")
    val mllib = lib("mllib")
    val sql = lib("sql")
  }
}
