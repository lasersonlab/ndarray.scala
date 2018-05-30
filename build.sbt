import settings._

scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  cdm,
  channel,
  junit,
  `google-cloud-nio`,
  spark.core,
  spark.mllib,
  spark.sql
)

resolvers += Resolver.mavenLocal
