import Dependencies._


ThisBuild / version := "0.1.0-SNAPSHOT"
lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "alandevlin",
      scalaVersion := "2.12.8",
      scalacOptions ++= Seq(
      "-Xfatal-warnings",  // New lines for each options
  "-deprecation",
  "-unchecked")
    )),
  name := "hangman",
  libraryDependencies += "dev.zio"  %% "zio" % "1.0.0-RC15",
  libraryDependencies += "org.scalaz"  %% "scalaz-core" % "7.3.0-M31"
)
