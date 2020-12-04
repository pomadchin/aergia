name := "aergia-alpha"

lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.12",
  crossScalaVersions := Seq("2.12.12", "2.13.4"),
  organization := "com.azavea",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-language:implicitConversions",
    "-language:reflectiveCalls",
    "-language:higherKinds",
    "-language:postfixOps",
    "-language:existentials",
    "-feature"
  ),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),

  fork := true,

  assembly / assemblyMergeStrategy := {
    case "reference.conf"                       => MergeStrategy.concat
    case "application.conf"                     => MergeStrategy.concat
    case n if n.startsWith("META-INF/services") => MergeStrategy.concat
    case n
      if n.endsWith(".SF") || n.endsWith(".RSA") || n.endsWith(".DSA") || n
        .endsWith(".semanticdb") =>
      MergeStrategy.discard
    case "META-INF/MANIFEST.MF" => MergeStrategy.discard
    case _                      => MergeStrategy.first
  },

  assembly / mainClass := Some("com.azavea.raster.LazyTest"),
  assembly / test := { },

  libraryDependencies ++= Seq(
    "org.locationtech.geotrellis" %% "geotrellis-raster"         % "3.5.1",
    "io.higherkindness"           %% "droste-core"               % "0.8.0",
    "org.locationtech.geotrellis" %% "geotrellis-raster-testkit" % "3.5.1" % Test,
    "org.scalatest"               %% "scalatest"                 % "3.2.3" % Test
  )
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .aggregate(core, benchmark)

lazy val core = project
  .settings(name := "aergia-core")
  .settings(commonSettings)

lazy val benchmark = project
  .dependsOn(core)
  .settings(name := "benchmark")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .enablePlugins(JmhPlugin)
  .settings(name := "benchmark", fork := true, javaOptions += "-Xmx4G")
