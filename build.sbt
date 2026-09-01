ThisBuild / organization := "net.ghoula"
ThisBuild / scalaVersion := "3.8.4"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

// Java 25
ThisBuild / javacOptions ++= Seq("--release", "25")
ThisBuild / javaOptions ++= Seq("-XX:+UseZGC")

// ===== Publishing Settings =====
//
// Maven Central (Central Portal) is the single publication target. Releases are staged locally
// and uploaded with `sonaRelease` (sbt 2.x built-in Central Portal support); artifacts are signed
// by sbt-pgp (`publishSigned`). Credentials are read automatically from SONATYPE_USERNAME /
// SONATYPE_PASSWORD.
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
ThisBuild / publishMavenStyle := true
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / licenses := Seq("GPL-3.0-or-later" -> uri("https://www.gnu.org/licenses/gpl-3.0.txt"))
ThisBuild / homepage := Some(uri("https://github.com/hakimjonas/sarati"))
ThisBuild / description := "Binary codec library and structural AST layer for Scala 3, with an XPath 1.0 evaluator."
ThisBuild / developers := List(
  Developer("hakimjonas", "Hakim Jonas Ghoula", "hakim@ghoula.net", uri("https://github.com/hakimjonas"))
)
ThisBuild / scmInfo := Some(
  ScmInfo(uri("https://github.com/hakimjonas/sarati"), "scm:git@github.com:hakimjonas/sarati.git")
)

lazy val sharedScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Werror",
  "-Wunused:all",
  "-Wrecurse-with-default",
  "-no-indent",
  "-language:strictEquality",
  "-Yexplicit-nulls",
  "-Wsafe-init"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "sarati",
    scalacOptions ++= sharedScalacOptions,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.3.5" % Test,
      "org.scalameta" %% "munit-scalacheck" % "1.3.0" % Test
    ),
    // Coverage floor: measured 70.69% stmt / 62.32% branch on 2026-08-28 (338 tests).
    // Thresholds sit just under the measured values so regressions fail CI without
    // being brittle against deterministic-test noise.
    coverageMinimumStmtTotal := 70.0,
    coverageMinimumBranchTotal := 60.0,
    coverageFailOnMinimum := true
  )

// JMH benchmarks against the codec (also the A/B pressure-test rig).
lazy val benchmarks = (project in file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "sarati-benchmarks",
    publish / skip := true,
    Jmh / sourceDirectory := (Compile / sourceDirectory).value,
    // NOTE: do NOT alias Jmh/classDirectory to Compile's, force Jmh/compile from
    // copyResources (deadlock: circular wait, sbt fails silently), or add the Jmh dir to
    // products (duplicate classes in packageBin). Under sbt 2 the Jmh-generated classes are
    // pruned from the Compile class directory anyway and sbt-assembly cannot see them; the
    // durable generator outputs are src_managed/jmh (sources) and resource_managed/jmh
    // (BenchmarkList). The CI smoke step compiles those sources against the fat jar itself
    // and overlays them in with `jar --update`.
    assembly / mainClass := Some("org.openjdk.jmh.Main"),
    assembly / assemblyJarName := "sarati-bench.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
      case PathList("META-INF", "versions", _*) => MergeStrategy.first
      case PathList("META-INF", _*) => MergeStrategy.first
      case "module-info.class" => MergeStrategy.discard
      case x if x.endsWith(".class") => MergeStrategy.first
      case _ => MergeStrategy.first
    }
  )
  .dependsOn(root)

// Command aliases
addCommandAlias("prepare", "scalafmtAll; scalafmtSbt; scalafixAll; Test/compile")
addCommandAlias("check", "scalafixAll --check; scalafmtCheckAll; scalafmtSbtCheck")
