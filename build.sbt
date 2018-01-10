name := "fla"

val scalazVersion     = "7.2.7"
val spireVersion      = "0.13.0"
val scalacheckVersion = "1.12.6"
val benchmarksVersion = "0.1.3-SNAPSHOT"
val cilibVersion      = "2.0.0-SNAPSHOT"
val shapelessVersion  = "2.3.2"

lazy val buildSettings = Seq(
  organization := "net.cilib",
  scalaVersion := "2.12.2",
  version := "0.0.2"
)

lazy val commonSettings = Seq(
  autoAPIMappings := true,
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:experimental.macros",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"
  ),
  coverageExcludedPackages := "fla\\.example\\..*",
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  libraryDependencies += compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
)

lazy val flaSettings = buildSettings ++ commonSettings

lazy val fla = project.in(file("."))
  .settings(flaSettings)
  .aggregate(example, metrics, tests, walks)
  .dependsOn(example, metrics, tests, walks)

lazy val walks = project
  .settings(flaSettings ++ Seq(
    moduleName := "fla-walks",
    libraryDependencies ++= Seq(
      "net.cilib" %% "cilib-core" % cilibVersion
    )
  ))

lazy val metrics = project
  .settings(flaSettings ++ Seq(
    moduleName := "fla-metrics",
    libraryDependencies ++= Seq(
      "net.cilib" %% "benchmarks" % benchmarksVersion,
      "net.cilib" %% "cilib-core" % cilibVersion,
      "net.cilib" %% "cilib-pso"  % cilibVersion
    )
  ))

lazy val example = project
  .dependsOn(metrics, walks)
  .settings(flaSettings ++ Seq(
    moduleName := "fla-example",
    libraryDependencies ++= Seq(
      "net.cilib"   %% "benchmarks" % benchmarksVersion,
      "net.cilib"   %% "cilib-core" % cilibVersion,
      "com.chuusai" %% "shapeless"  % shapelessVersion
    )
  ))

lazy val tests = project
  .dependsOn(metrics, walks)
  .settings(flaSettings ++ Seq(
    moduleName := "fla-tests",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck"                % scalacheckVersion % "test",
      "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion     % "test"
    )
  ))
