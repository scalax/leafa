scalaVersion := "2.12.7"

scalacOptions ++= Seq("-feature", "-deprecation", "-Ywarn-unused-import")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.6" cross CrossVersion.binary)

val asuna = (project in file("."))

scalafmtOnCompile := true