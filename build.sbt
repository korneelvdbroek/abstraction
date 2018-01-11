name := "abstraction"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-optimise")

libraryDependencies ++= Seq(
  "org.mockito" % "mockito-core" % "1.10.17" % "test",
  "org.scalatest" %% "scalatest" % "2.2.5",
  "org.pegdown" % "pegdown" % "1.4.2" % "test",

  // Last stable release
  "org.scalanlp" %% "breeze" % "0.13.1",

  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.13.1",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "0.13.1",

  // Apache Spark
  "org.apache.spark" %% "spark-sql" % "2.2.0" excludeAll(
    ExclusionRule(name = "log4j"),
    ExclusionRule(name = "slf4j-log4j12")
  ),
  "org.apache.spark" %% "spark-mllib" % "2.2.0" excludeAll(
    ExclusionRule(name = "log4j"),
    ExclusionRule(name = "slf4j-log4j12")
  ),

  // Logging dependencies
  "ch.qos.logback" % "logback-core" % "1.2.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.slf4j" % "log4j-over-slf4j" % "1.7.25",
  "org.clapper" %% "grizzled-scala" % "1.3",
  "org.clapper" %% "grizzled-slf4j" % "1.3.1"
)
