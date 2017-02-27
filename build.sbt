name := "fla"

val scalazVersion     = "7.2.7"
val spireVersion      = "0.13.0"
val scalacheckVersion = "1.12.6"

organization := "net.cilib"

version := "0.0.1"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core" % scalazVersion,
  "org.spire-math" %% "spire"       % spireVersion,
  "net.cilib"      %% "benchmarks" % "0.1.1",
  "net.cilib"      %% "cilib-core" % "2.0.0-M3",
  "net.cilib"      %% "cilib-pso" % "2.0.0-M3",
  "net.cilib"      %% "cilib-exec" % "2.0.0-M3",
  "org.scalacheck" %% "scalacheck" % scalacheckVersion % "test",
  "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion % "test"
)
