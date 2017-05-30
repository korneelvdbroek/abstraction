name := "abstraction"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.mockito" % "mockito-core" % "1.10.17" % "test",
  "org.scalatest" %% "scalatest" % "2.2.5",
  "org.pegdown" % "pegdown" % "1.4.2" % "test"
)
