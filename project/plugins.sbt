addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.7")
addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.1.1")

// Signing for Maven Central (`publishSigned`).
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.2")

// Coverage
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")

// Benchmarking
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.5.0")
